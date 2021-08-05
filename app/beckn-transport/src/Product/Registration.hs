module Product.Registration (checkPersonExists, initiateLogin, login, reInitiateLogin, logout) where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation (runRequestValidation)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Registration
import Types.Error
import Types.Metrics (CoreMetrics)
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
      _ -> throwError $ InvalidRequest "medium and type fields must be SMS and OTP"

initiateFlowHitsCountKey :: SP.Person -> Text
initiateFlowHitsCountKey person = "Registration:initiateFlow" <> getId person.id <> ":hitsCount"

initiateFlow ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["otpSmsTemplate" ::: Text],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  InitiateLoginReq ->
  SmsConfig ->
  m InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  mbPerson <- QP.findByMobileNumber countryCode mobileNumber
  person <- case mbPerson of
    Nothing -> do
      when (req.role == SP.DRIVER) $ throwError $ InvalidRequest "Driver must be registered by Transport Admin"
      createPerson req
    Just p -> pure p
  checkSlidingWindowLimit (initiateFlowHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId SR.USER (show <$> useFakeOtpM)
      QR.create token
      return token
    Nothing -> do
      token <- makeSession scfg req entityId SR.USER Nothing
      QR.create token
      otpSmsTemplate <- asks (.otpSmsTemplate)
      withLogTag ("personId_" <> getId person.id) $
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
      return token
  let attempts = SR.attempts regToken
      tokenId = SR.id regToken
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: EncFlow m r => InitiateLoginReq -> m SP.Person
makePerson req = do
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
        role = req.role,
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
        udf1 = Nothing,
        udf2 = Nothing,
        deviceToken = req.deviceToken,
        organizationId = Nothing,
        locationId = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  InitiateLoginReq ->
  Text ->
  SR.RTEntityType ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId entityType fakeOtp = do
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
        entityType = entityType,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

loginHitsCountKey :: Id SP.Person -> Text
loginHitsCountKey id = "Registration:login:" <> getId id <> ":hitsCount"

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandlerAPI $ do
    runRequestValidation validateLoginReq req
    regToken@SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    checkSlidingWindowLimit (loginHitsCountKey $ Id entityId)
    when verified $ throwError $ AuthBlocked "Already verified."
    checkForExpiry authExpiry updatedAt
    let isValid =
          authMedium == req.medium && authType == req.__type
            && authValueHash
              == req.hash
    if isValid
      then do
        person <- checkPersonExists entityId
        clearOldRegToken person $ Id tokenId
        QR.updateVerified tokenId True
        let deviceToken = (req.deviceToken) <|> (person.deviceToken)
        QP.update person.id SP.ACTIVE True deviceToken
        updatedPerson <-
          QP.findPersonById person.id
            >>= fromMaybeM PersonNotFound
        when (person.status == SP.INACTIVE) $
          Notify.notifyOnRegistration regToken updatedPerson
        decPerson <- decrypt updatedPerson
        return $ LoginRes token (Just . makeUserInfoRes $ SP.maskPerson decPerson)
      else throwError InvalidAuthData
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

checkRegistrationTokenExists :: DBFlow m r => Text -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM (TokenNotFound tokenId)

createPerson :: (DBFlow m r, EncFlow m r) => InitiateLoginReq -> m SP.Person
createPerson req = do
  person <- makePerson req
  QP.createFlow person
  pure person

checkPersonExists :: DBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  QP.findPersonById (Id entityId) >>= fromMaybeM PersonNotFound

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandlerAPI $ do
    runRequestValidation validateReInitiateLoginReq req
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    void $ checkPersonExists entityId
    if attempts > 0
      then do
        smsCfg <- smsCfg <$> ask
        otpSmsTemplate <- otpSmsTemplate <$> ask
        let mobileNumber = req.mobileNumber
            countryCode = req.mobileCountryCode
        withLogTag ("personId_" <> entityId) $
          SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
        _ <- QR.updateAttempts (attempts - 1) id
        return $ InitiateLoginRes tokenId (attempts - 1)
      else throwError $ AuthBlocked "Limit exceeded."

clearOldRegToken :: DBFlow m r => SP.Person -> Id SR.RegistrationToken -> m ()
clearOldRegToken person = QR.deleteByEntitiyIdExceptNew (getId $ person.id)

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI $ do
  regTokens <- QR.findAllByPersonId personId
  -- We should have only one RegToken at this point, but just in case we use findAll
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key
  uperson <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  DB.runSqlDBTransaction $ do
    QP.updatePersonRec (uperson.id) uperson {SP.deviceToken = Nothing}
    QR.deleteByEntitiyId $ getId personId
  pure Success
