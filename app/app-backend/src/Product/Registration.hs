module Product.Registration (initiateLogin, login, reInitiateLogin, logout) where

import App.Types
import Beckn.External.Encryption (decrypt, encrypt)
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation (runRequestValidation)
import qualified Crypto.Number.Generate as Cryptonite
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.Registration
import Types.Error
import Types.Metrics
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR
import Utils.Auth (authTokenCacheKey)
import Utils.Common
import qualified Utils.Notifications as Notify

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandlerAPI $ do
    runRequestValidation validateInitiateLoginReq req
    case (req.medium, req.__type) of
      (SR.SMS, SR.OTP) -> ask >>= initiateFlow req . smsCfg
      _ -> throwError $ InvalidRequest "medium and type must be sms and otp respectively"

initiateFlowHitsCountKey :: SP.Person -> Text
initiateFlowHitsCountKey person = "Registration:initiateFlow" <> getId person.id <> ":hitsCount"

initiateFlow ::
  ( EncFlow m r,
    DBFlow m r,
    FCMFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions, "otpSmsTemplate" ::: Text],
    CoreMetrics m
  ) =>
  InitiateLoginReq ->
  SmsConfig ->
  m InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  person <-
    Person.findByRoleAndMobileNumber SP.USER countryCode mobileNumber
      >>= maybe (createPerson req) return
  checkSlidingWindowLimit (initiateFlowHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId (show <$> useFakeOtpM)
      DB.runSqlDB (RegistrationToken.create token)
      return token
    Nothing -> do
      token <- makeSession scfg req entityId Nothing
      DB.runSqlDB (RegistrationToken.create token)
      otpSmsTemplate <- asks (.otpSmsTemplate)
      withLogTag ("personId_" <> getId person.id) $
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
      return token
  let attempts = SR.attempts regToken
      tokenId = SR.id regToken
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: EncFlow m r => InitiateLoginReq -> m SP.Person
makePerson req = do
  role <- (req.role) & fromMaybeM (InvalidRequest "You should pass person's role.")
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt $ Just req.mobileNumber
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        fullName = Nothing,
        role = role,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        mobileNumber = encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        verified = False,
        status = SP.INACTIVE,
        deviceToken = req.deviceToken,
        udf1 = Nothing,
        udf2 = Nothing,
        organizationId = Nothing,
        locationId = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  DBFlow m r =>
  SmsSessionConfig ->
  InitiateLoginReq ->
  Text ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = rtid,
        token = token,
        attempts = attempts,
        authMedium = req.medium,
        authType = req.__type,
        authValueHash = otp,
        verified = False,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

loginHitsCountKey :: Id SP.Person -> Text
loginHitsCountKey id = "Registration:login:" <> getId id <> ":hitsCount"

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandlerAPI $ do
    runRequestValidation validateLoginReq req
    regToken@SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    checkSlidingWindowLimit (loginHitsCountKey $ Id entityId)
    when verified $ throwError $ AuthBlocked "Already verified."
    checkForExpiry authExpiry updatedAt
    let isValid =
          authMedium == req.medium
            && authType == req.__type
            && authValueHash == req.hash
    if isValid
      then do
        person <- checkPersonExists entityId
        clearOldRegToken person $ Id tokenId
        let personId = person.id
            updatedPerson =
              person{status = SP.ACTIVE,
                     deviceToken =
                       (req.deviceToken) <|> (person.deviceToken)
                    }
        when (person.status == SP.INACTIVE) $
          Notify.notifyOnRegistration regToken updatedPerson
        DB.runSqlDB (Person.updateMultiple personId updatedPerson)
        LoginRes token . makeUserInfoRes . SP.maskPerson
          <$> ( Person.findById personId
                  >>= fromMaybeM PersonNotFound
                  >>= decrypt
              )
      else throwError InvalidAuthData
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

getRegistrationTokenE :: DBFlow m r => Text -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound tokenId)

createPerson :: (EncFlow m r, DBFlow m r) => InitiateLoginReq -> m SP.Person
createPerson req = do
  person <- makePerson req
  Person.create person
  pure person

checkPersonExists :: DBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM PersonDoesNotExist

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandlerAPI $ do
    runRequestValidation validateReInitiateLoginReq req
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    void $ checkPersonExists entityId
    if attempts > 0
      then do
        smsCfg <- smsCfg <$> ask
        otpSmsTemplate <- otpSmsTemplate <$> ask
        let mobileNumber = req.mobileNumber
            countryCode = req.mobileCountryCode
        withLogTag ("personId_" <> entityId) $
          SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
        _ <- RegistrationToken.updateAttempts (attempts - 1) id
        return $ InitiateLoginRes tokenId (attempts - 1)
      else throwError $ AuthBlocked "Attempts limit exceed."

clearOldRegToken :: DBFlow m r => SP.Person -> Id SR.RegistrationToken -> m ()
clearOldRegToken person = RegistrationToken.deleteByPersonIdExceptNew (getId $ person.id)

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI $ do
  regTokens <- RegistrationToken.findAllByPersonId personId
  -- We should have only one RegToken at this point, but just in case we use findAll
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key
  DB.runSqlDBTransaction $ do
    Person.updateDeviceToken personId Nothing
    RegistrationToken.deleteByPersonId personId
  pure Success
