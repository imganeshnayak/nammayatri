{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
  ( DriverRCReq (..),
    DriverRCRes,
    verifyRC,
    onVerifyRC,
    convertUTCTimetoDate,
  )
where

import AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Control.Applicative ((<|>))
import Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.DriverOnboarding.DriverRCAssociation as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Person as Person
import Environment
import qualified Idfy.Flow as Idfy
import qualified Idfy.Types as Idfy
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.Person as Person

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

verifyRC ::
  Id Person.Person ->
  DriverRCReq ->
  Flow DriverRCRes
verifyRC personId req@DriverRCReq {..} = do
  runRequestValidation validateDriverRCReq req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
  unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
  unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
  unless (imageMetadata.imageType == Image.VehicleRegistrationCertificate) $
    throwError (ImageInvalidType (show Image.VehicleRegistrationCertificate) (show imageMetadata.imageType))

  configs <- asks (.driverOnboardingConfigs)
  when (isNothing dateOfRegistration && configs.checkImageExtraction) $ do
    image <- S3.get (T.unpack imageMetadata.s3Path)
    resp <- Idfy.extractRCImage image Nothing
    case resp.result of
      Just result -> do
        let extractRCNumber = removeSpaceAndDash <$> result.extraction_output.registration_number
        let rcNumber = removeSpaceAndDash <$> Just vehicleRegistrationCertNumber
        unless (extractRCNumber == rcNumber) $
          throwImageError imageId $ ImageDocumentNumberMismatch (maybe "null" maskText extractRCNumber) (maybe "null" maskText rcNumber)
      Nothing -> throwImageError imageId ImageExtractionFailed

  now <- getCurrentTime
  mDriverAssociation <- DAQuery.getActiveAssociationByDriver personId

  case mDriverAssociation of
    Just driverAssociaion -> do
      driverRC <- RCQuery.findById driverAssociaion.rcId >>= fromMaybeM (InvalidRequest "Missing RC entry")
      rcNumber <- decrypt driverRC.certificateNumber
      unless (rcNumber == vehicleRegistrationCertNumber) $ throwImageError imageId DriverAlreadyLinked
      unless (driverRC.fitnessExpiry < now) $ throwImageError imageId RCAlreadyUpdated -- RC not expired
      verifyRCFlow personId vehicleRegistrationCertNumber imageId dateOfRegistration
    Nothing -> do
      mVehicleRC <- RCQuery.findLastVehicleRC vehicleRegistrationCertNumber
      case mVehicleRC of
        Just vehicleRC -> do
          mRCAssociation <- DAQuery.getActiveAssociationByRC vehicleRC.id
          when (isJust mRCAssociation) $ throwImageError imageId RCAlreadyLinked
          verifyRCFlow personId vehicleRegistrationCertNumber imageId dateOfRegistration
        Nothing -> do
          verifyRCFlow personId vehicleRegistrationCertNumber imageId dateOfRegistration

  return Success

verifyRCFlow :: Id Person.Person -> Text -> Id Image.Image -> Maybe UTCTime -> Flow ()
verifyRCFlow personId rcNumber imageId dateOfRegistration = do
  now <- getCurrentTime
  encryptedRC <- encrypt rcNumber
  configs <- asks (.driverOnboardingConfigs)
  let imageExtractionValidation =
        if isNothing dateOfRegistration && configs.checkImageExtraction
          then Domain.Success
          else Domain.Skipped
  idfyRes <- Idfy.verifyRC rcNumber
  idfyVerificationEntity <- mkIdfyVerificationEntity idfyRes.request_id now imageExtractionValidation encryptedRC
  runTransaction $ IVQuery.create idfyVerificationEntity
  where
    mkIdfyVerificationEntity requestId now imageExtractionValidation encryptedRC = do
      id <- generateGUID
      return $
        Domain.IdfyVerification
          { id,
            driverId = personId,
            documentImageId1 = imageId,
            documentImageId2 = Nothing,
            requestId,
            docType = Image.VehicleRegistrationCertificate,
            documentNumber = encryptedRC,
            imageExtractionValidation = imageExtractionValidation,
            issueDateOnDoc = dateOfRegistration,
            status = "pending",
            idfyResponse = Nothing,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Domain.IdfyVerification -> Idfy.RCVerificationOutput -> Flow AckResponse
onVerifyRC verificationReq output = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)

  if verificationReq.imageExtractionValidation == Domain.Skipped
    && isJust verificationReq.issueDateOnDoc
    && ( (convertUTCTimetoDate <$> verificationReq.issueDateOnDoc)
           /= (convertUTCTimetoDate <$> (convertTextToUTC output.registration_date))
       )
    then runTransaction $ IVQuery.updateExtractValidationStatus verificationReq.requestId Domain.Failed >> return Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      driverOnboardingConfigs <- asks (.driverOnboardingConfigs)

      mEncryptedRC <- encrypt `mapM` output.registration_number
      let mbFitnessEpiry = convertTextToUTC output.fitness_upto
      let mVehicleRC = createRC driverOnboardingConfigs output id verificationReq.documentImageId1 now <$> mEncryptedRC <*> mbFitnessEpiry

      case mVehicleRC of
        Just vehicleRC -> do
          runTransaction $ RCQuery.upsert vehicleRC

          -- linking to driver
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (InternalError "RC not found")
          mRCAssociation <- DAQuery.getActiveAssociationByRC rc.id
          when (isNothing mRCAssociation) $ do
            currAssoc <- DAQuery.getActiveAssociationByDriver person.id
            when (isJust currAssoc) $ do runTransaction $ DAQuery.endAssociation person.id
            driverRCAssoc <- mkAssociation person.id rc.id
            runTransaction $ DAQuery.create driverRCAssoc
          return Ack
        _ -> return Ack
  where
    mkAssociation driverId rcId = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.DriverRCAssociation
          { id,
            driverId,
            rcId,
            associatedOn = now,
            associatedTill = convertTextToUTC (Just "2099-12-12"),
            consent = True,
            consentTimestamp = now
          }

createRC ::
  DriverOnboardingConfigs ->
  Idfy.RCVerificationOutput ->
  Id Domain.VehicleRegistrationCertificate ->
  Id Image.Image ->
  UTCTime ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.VehicleRegistrationCertificate
createRC configs output id imageId now edl expiry = do
  let insuranceValidity = convertTextToUTC output.insurance_validity
  let vehicleClass = output.vehicle_class
  let vehicleCapacity = maybe (Just 3) (readMaybe . T.unpack) output.seating_capacity
  let verificationStatus = validateRCStatus configs expiry insuranceValidity vehicleClass now vehicleCapacity
  Domain.VehicleRegistrationCertificate
    { id,
      documentImageId = imageId,
      certificateNumber = edl,
      fitnessExpiry = expiry,
      permitExpiry = convertTextToUTC output.permit_validity_upto,
      pucExpiry = convertTextToUTC output.puc_validity_upto,
      vehicleClass,
      vehicleManufacturer = output.manufacturer <|> output.manufacturer_model,
      vehicleCapacity,
      vehicleModel = output.m_y_manufacturing <|> output.manufacturer_model,
      vehicleColor = output.color <|> output.colour,
      vehicleEnergyType = output.fuel_type,
      insuranceValidity,
      verificationStatus,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }

validateRCStatus :: DriverOnboardingConfigs -> UTCTime -> Maybe UTCTime -> Maybe Text -> UTCTime -> Maybe Int -> Domain.VerificationStatus
validateRCStatus configs expiry insuranceValidity cov now capacity = do
  let validCOV = (not configs.checkRCVehicleClass) || maybe False (isValidCOVRC capacity) cov
  let validInsurance = (not configs.checkRCInsuranceExpiry) || maybe False (now <) insuranceValidity
  if ((not configs.checkRCExpiry) || now < expiry) && validCOV && validInsurance then Domain.VALID else Domain.INVALID

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Maybe Int -> Text -> Bool
isValidCOVRC capacity cov = T.isInfixOf "3WT" cov || (T.isInfixOf "Passenger" cov && capacity == Just 4) || T.isInfixOf "3WN" cov

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

convertUTCTimetoDate :: UTCTime -> Text
convertUTCTimetoDate utctime = T.pack (DT.formatTime DT.defaultTimeLocale "%d/%m/%Y" utctime)
