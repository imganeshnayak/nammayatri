{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver
  ( driverDocumentsInfo,
    driverAadhaarInfo,
    listDrivers,
    driverActivity,
    enableDriver,
    disableDriver,
    blockDriverWithReason,
    blockDriver,
    blockReasonList,
    unblockDriver,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    unlinkDL,
    unlinkAadhaar,
    endRCAssociation,
    updatePhoneNumber,
    addVehicle,
    updateDriverName,
    clearOnRideStuckDrivers,
    getDriverDue,
    collectCash,
    exemptCash,
    driverAadhaarInfoByPhone,
    updateByPhoneNumber,
    setRCStatus,
    deleteRC,
    getDriverHomeLocation,
    updateDriverHomeLocation,
    incrementDriverGoToCount,
    getDriverGoHomeInfo,
    getPaymentHistoryEntityDetails,
    getPaymentHistory,
    addVehicleForFleet,
    getAllVehicleForFleet,
    fleetUnlinkVehicle,
    fleetRemoveVehicle,
    fleetTotalEarning,
    fleetVehicleEarning,
    fleetDriverEarning,
    updateSubscriptionDriverFeeAndInvoice,
    setVehicleDriverRcStatusForFleet,
    fleetRemoveDriver,
    getFleetDriverVehicleAssociation,
    getFleetDriverAssociation,
    getFleetVehicleAssociation,
    getAllDriverForFleet,
    sendSmsToDriver,
    SendSmsReq (..),
    MediaChannel (..),
    VolunteerTransactionStorageReq (..),
  )
where

import Control.Applicative ((<|>))
import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AVD
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest (CachedGoHomeRequest (..))
import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as DDHL
import qualified Domain.Types.DriverBlockReason as DBR
import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import Domain.Types.DriverOnboarding.Image (Image)
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.FleetDriverAssociation
import qualified Domain.Types.FleetDriverAssociation as DTFDA
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.MerchantMessage (MessageKey)
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig (DashboardMediaSendingLimit (..))
import qualified Domain.Types.Message.Message as Domain
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Kernel.Utils.Validation (runRequestValidation)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.DeleteDriver as DeleteDriver
import qualified SharedLogic.DriverFee as SLDriverFee
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQGHC
import Storage.CachedQueries.DriverBlockReason as DBR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Driver.GoHomeFeature.DriverHomeLocation as QDHL
import Storage.Queries.DriverFee (findPendingFeesByDriverId)
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as AV
import qualified Storage.Queries.DriverOnboarding.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverOnboarding.Status as QDocStatus
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Message.Message as MQuery
import qualified Storage.Queries.Message.MessageTranslation as MTQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import Storage.Queries.Ride as QRide
import qualified Storage.Queries.Ride as QRD
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

-- FIXME: not tested yet because of no onboarding test data
driverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCity.id >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  let onboardingTryLimit = transporterConfig.onboardingTryLimit
  drivers <- B.runInReplica $ QDocStatus.fetchDriverDocsInfo merchant merchantOpCity Nothing
  pure $ foldl' (func onboardingTryLimit now) Common.emptyInfo drivers
  where
    oneMonth :: NominalDiffTime
    oneMonth = 60 * 60 * 24 * 30
    func :: Int -> UTCTime -> Common.DriverDocumentsInfoRes -> QDocStatus.DriverDocsInfo -> Common.DriverDocumentsInfoRes
    func onboardingTryLimit now acc fd = do
      let mbLic = fd.license
          mbRegCert = fd.assocReg
          mbLicReq = fd.licenseVerificationReq
          mbVehRegReq = fd.regVerificationReq

          dlStatus = getLicenseStatus onboardingTryLimit fd.numLicenseImages mbLic mbLicReq
          rcStatus = getRegCertStatus onboardingTryLimit fd.numVehRegImages mbRegCert mbVehRegReq

          mbDlExpiration = (.licenseExpiry) <$> mbLic
          mbRcExpiration = getRcExpiration . snd <$> mbRegCert
          dlExpiresInMonth = expiresInMonth mbDlExpiration
          rcExpiresInMonth = expiresInMonth mbRcExpiration
          expiresInMonth mbExp = fromMaybe False $
            flip fmap mbExp $
              \exp_ -> let dif = diffUTCTime exp_ now in dif < oneMonth && dif > 0

      acc{Common.registered = acc.registered + 1,
          Common.verified = incrIf fd.driverInfo.verified acc.verified,
          Common.enabled = incrIf fd.driverInfo.enabled acc.enabled,
          Common.blocked = incrIf (not fd.driverInfo.blocked) acc.blocked,
          Common.validDocuments = incrDocs (dlStatus == VALID) (rcStatus == VALID) acc.validDocuments,
          Common.subscribed = incrIf fd.driverInfo.subscribed acc.subscribed,
          Common.invalidDocuments = incrDocs (dlStatus == INVALID) (rcStatus == INVALID) acc.invalidDocuments,
          Common.verificationPending = incrDocs (dlStatus == PENDING) (rcStatus == PENDING) acc.verificationPending,
          Common.verificationFailed = incrDocs (dlStatus == FAILED) (rcStatus == FAILED) acc.verificationFailed,
          Common.verificationLimitExceeded = incrDocs (dlStatus == LIMIT_EXCEED) (rcStatus == LIMIT_EXCEED) acc.verificationLimitExceeded,
          Common.docsExpiringInMonth = incrDocs dlExpiresInMonth rcExpiresInMonth acc.docsExpiringInMonth
         }

incrIf :: Num a => Bool -> a -> a
incrIf b = if b then (+ 1) else identity

incrDocs :: Bool -> Bool -> Common.DocumentsByStateInfo -> Common.DocumentsByStateInfo
incrDocs lic vehReg old =
  old{Common.driverLicense = incrIf lic old.driverLicense,
      Common.vehicleRegistrationCertificate = incrIf vehReg old.vehicleRegistrationCertificate
     }

getRcExpiration :: VehicleRegistrationCertificate -> UTCTime
getRcExpiration = (.fitnessExpiry)

getLicenseStatus :: Int -> Int -> Maybe DriverLicense -> Maybe IV.IdfyVerification -> St.ResponseStatus
getLicenseStatus onboardingTryLimit currentTries mbLicense mbLicReq =
  case mbLicense of
    Just driverLicense -> St.mapStatus driverLicense.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbLicReq

getRegCertStatus :: Int -> Int -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate) -> Maybe IV.IdfyVerification -> St.ResponseStatus
getRegCertStatus onboardingTryLimit currentTries mbRegCert mbVehRegReq =
  case mbRegCert of
    Just (_assoc, vehicleRC) -> St.mapStatus vehicleRC.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbVehRegReq

---------

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

---------------------------------------------------------------------
listDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
listDrivers merchantShortId opCity mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhone mbVehicleNumberSearchString = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  driversWithInfo <- B.runInReplica $ QPerson.findAllDriversWithInfoAndVehicle merchant.id merchantOpCity limit offset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString
  items <- mapM buildDriverListItem driversWithInfo
  let count = length items
  -- should we consider filters in totalCount, e.g. count all enabled drivers?
  -- totalCount <- Esq.runInReplica $ QPerson.countDrivers merchant.id
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.DriverListRes {totalItems = count, summary, drivers = items}
  where
    maxLimit = 20
    defaultLimit = 10

buildDriverListItem :: EncFlow m r => (DP.Person, DrInfo.DriverInformation, Maybe DVeh.Vehicle) -> m Common.DriverListItem
buildDriverListItem (person, driverInformation, mbVehicle) = do
  phoneNo <- mapM decrypt person.mobileNumber
  pure $
    Common.DriverListItem
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        vehicleNo = mbVehicle <&> (.registrationNo),
        phoneNo,
        enabled = driverInformation.enabled,
        blocked = driverInformation.blocked,
        verified = driverInformation.verified,
        subscribed = driverInformation.subscribed,
        onRide = driverInformation.onRide,
        active = driverInformation.active,
        onboardingDate = driverInformation.lastEnabledOn
      }

---------------------------------------------------------------------
getDriverDue :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Text -> Flow [Common.DriverOutstandingBalanceResp] -- add mig and totalFee
getDriverDue merchantShortId _ mbMobileCountryCode phone = do
  let mobileCountryCode = fromMaybe "+91" mbMobileCountryCode
  merchant <- findMerchantByShortId merchantShortId
  mobileNumber <- getDbHash phone
  driver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchant mobileCountryCode mobileNumber merchant.id >>= fromMaybeM (InvalidRequest "Person not found")
  driverFees <- findPendingFeesByDriverId (cast driver.id)
  driverFeeByInvoices <- case driverFees of
    [] -> pure []
    _ -> SLDriverFee.groupDriverFeeByInvoices driverFees
  return $ map (mkPaymentDueResp driver.id) driverFeeByInvoices
  where
    mkPaymentDueResp driverId SLDriverFee.DriverFeeByInvoice {..} = do
      let platformFee_ = mkPlatformFee platformFee
          status_ = castStatus status
          driverFeeId = cast invoiceId
          driverId_ = cast driverId
      Common.DriverOutstandingBalanceResp
        { govtCharges = round govtCharges,
          platformFee = platformFee_,
          status = status_,
          driverId = driverId_,
          ..
        }

    mkPlatformFee SLDriverFee.PlatformFee {..} = Common.PlatformFee {..}

    castStatus status = case status of -- only PENDING and OVERDUE possible
      ONGOING -> Common.ONGOING
      PAYMENT_PENDING -> Common.PAYMENT_PENDING
      PAYMENT_OVERDUE -> Common.PAYMENT_OVERDUE
      CLEARED -> Common.CLEARED
      EXEMPTED -> Common.EXEMPTED
      COLLECTED_CASH -> Common.COLLECTED_CASH
      INACTIVE -> Common.INACTIVE
      CLEARED_BY_YATRI_COINS -> Common.CLEARED_BY_YATRI_COINS

---------------------------------------------------------------------
driverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && driver.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  unless (driverInf.aadhaarVerified) $ throwError $ InvalidRequest "Person aadhaar verification is pending"
  res <- AV.findByDriverId personId
  case res of
    Just aadhaarData -> do
      pure
        Common.DriverAadhaarInfoRes
          { driverName = aadhaarData.driverName,
            driverGender = aadhaarData.driverGender,
            driverDob = aadhaarData.driverDob,
            driverImage = aadhaarData.driverImage
          }
    Nothing -> throwError $ InvalidRequest "no aadhaar data is found"

---------------------------------- -----------------------------------
driverActivity :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverActivityRes
driverActivity merchantShortId _ = do
  merchant <- findMerchantByShortId merchantShortId
  Common.mkDriverActivityRes <$> B.runInReplica (QDriverInfo.countDrivers merchant.id)

---------------------------------------------------------------------
enableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
enableDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  mVehicle <- QVehicle.findById personId
  linkedRCs <- QRCAssociation.findAllLinkedByDriverId personId

  when (isNothing mVehicle && null linkedRCs) $
    throwError (InvalidRequest "Can't enable driver if no vehicle or no RCs are linked to them")

  QDriverInfo.updateEnabledState driverId True
  logTagInfo "dashboard -> enableDriver : " (show personId)
  fork "sending dashboard sms - onboarding" $ do
    Sms.sendDashboardSms merchant.id merchantOpCityId Sms.ONBOARDING Nothing personId Nothing 0
  pure Success

---------------------------------------------------------------------
disableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
disableDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  QDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> disableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------

blockDriverWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.BlockDriverWithReasonReq -> Flow APISuccess
blockDriverWithReason merchantShortId opCity reqDriverId dashboardUserName req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    B.runInReplica (QPerson.findById personId)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when (driverInf.blocked) $ throwError DriverAccountAlreadyBlocked
  QDriverInfo.updateDynamicBlockedState driverId req.blockReason req.blockTimeInHours dashboardUserName True
  maxShards <- asks (.maxShards)
  case req.blockTimeInHours of
    Just hrs -> do
      let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral hrs) * 60 * 60
      JC.createJobIn @_ @'UnblockDriver unblockDriverJobTs maxShards $
        UnblockDriverRequestJobData
          { driverId = driverId
          }
    Nothing -> return ()
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
--TODO : To Be Deprecated

blockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
blockDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && driver.merchantOperatingCityId == merchantOpCityId) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when (not driverInf.blocked) (void $ QDriverInfo.updateBlockedState driverId True Nothing)
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------

blockReasonList :: Flow [Common.BlockReason]
blockReasonList = do
  convertToBlockReasonList <$> DBR.findAll

convertToBlockReasonList :: [DBR.DriverBlockReason] -> [Common.BlockReason]
convertToBlockReasonList = map convertToCommon

convertToCommon :: DBR.DriverBlockReason -> Common.BlockReason
convertToCommon res =
  Common.BlockReason
    { reasonCode = cast res.reasonCode,
      blockReason = res.blockReason,
      blockTimeInHours = res.blockTimeInHours
    }

---------------------------------------------------------------------
collectCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
collectCash = recordPayment False

---------------------------------------------------------------------

exemptCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
exemptCash = recordPayment True

---------------------------------------------------------------------

paymentStatus :: Bool -> DriverFeeStatus
paymentStatus isExempted
  | isExempted = EXEMPTED
  | otherwise = COLLECTED_CASH

recordPayment :: Bool -> ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
recordPayment isExempted merchantShortId opCity reqDriverId requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica (QPerson.findById personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  driverFees <- findPendingFeesByDriverId driverId
  let totalFee = sum $ map (\fee -> fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst) driverFees
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  QDriverInfo.updatePendingPayment False driverId
  QDriverInfo.updateSubscription True driverId
  mapM_ (QDF.updateCollectedPaymentStatus (paymentStatus isExempted) (Just requestorId) now) ((.id) <$> driverFees)
  invoices <- (B.runInReplica . QINV.findActiveManualOrMandateSetupInvoiceByFeeId . (.id)) `mapM` driverFees
  mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.SUCCESS . (.id)) (concat invoices)
  fork "sending dashboard sms - collected cash" $ do
    Sms.sendDashboardSms merchantId merchantOpCityId Sms.CASH_COLLECTED Nothing personId Nothing totalFee
  pure Success

---------------------------------------------------------------------
unblockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
unblockDriver merchantShortId opCity reqDriverId dashboardUserName = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInf.blocked (void $ QDriverInfo.updateBlockedState driverId False (Just dashboardUserName))
  logTagInfo "dashboard -> unblockDriver : " (show personId)
  pure Success

driverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
driverLocation merchantShortId _ mbLimit mbOffset req = do
  merchant <- findMerchantByShortId merchantShortId
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

buildDriverLocationListItem :: EncFlow m r => QPerson.FullDriver -> m Common.DriverLocationItem
buildDriverLocationListItem f = do
  let p = f.person
      v = f.vehicle
  phoneNo <- maybe (pure "") decrypt p.mobileNumber
  pure
    Common.DriverLocationItem
      { driverId = cast p.id,
        firstName = p.firstName,
        middleName = p.middleName,
        lastName = p.lastName,
        vehicleNo = v.registrationNo,
        phoneNo,
        active = f.info.active,
        onRide = f.info.onRide,
        location = LatLong f.location.lat f.location.lon,
        lastLocationTimestamp = f.location.coordinatesCalculatedAt
      }

---------------------------------------------------------------------
mobileIndianCode :: Text
mobileIndianCode = "+91"

driverInfo :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> Flow Common.DriverInfoRes
driverInfo merchantShortId _ mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId mbFleet = do
  when mbFleet $ do
    when (isNothing mbVehicleNumber) $ throwError $ InvalidRequest "Fleet Owner can only search with vehicle Number"
    vehicleInfo <- RCQuery.findLastVehicleRCFleet' (fromMaybe " " mbVehicleNumber) fleetOwnerId
    when (isNothing vehicleInfo) $ throwError $ InvalidRequest "Fleet Owner does not have a vehicle linked with this vehicle number"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  merchant <- findMerchantByShortId merchantShortId
  driverWithRidesCount <- case (mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber) of
    (Just mobileNumber, Nothing, Nothing, Nothing) -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      let mobileCountryCode = fromMaybe mobileIndianCode mbMobileCountryCode
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id (Just (mobileNumberDbHash, mobileCountryCode)) Nothing Nothing Nothing
          >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
    (Nothing, Just vehicleNumber, Nothing, Nothing) -> do
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing (Just vehicleNumber) Nothing Nothing
          >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    (Nothing, Nothing, Just driverLicenseNumber, Nothing) -> do
      dlNumberHash <- getDbHash driverLicenseNumber
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing Nothing (Just dlNumberHash) Nothing
          >>= fromMaybeM (InvalidRequest "License does not exist.")
    (Nothing, Nothing, Nothing, Just rcNumber) -> do
      rcNumberHash <- getDbHash rcNumber
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing Nothing Nothing (Just rcNumberHash)
          >>= fromMaybeM (InvalidRequest "Registration certificate does not exist.")
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\" is required"
  let driverId = driverWithRidesCount.person.id
  mbDriverLicense <- B.runInReplica $ QDriverLicense.findByDriverId driverId
  rcAssociationHistory <- B.runInReplica $ QRCAssociation.findAllByDriverId driverId
  buildDriverInfoRes driverWithRidesCount mbDriverLicense rcAssociationHistory

buildDriverInfoRes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  QPerson.DriverWithRidesCount ->
  Maybe DriverLicense ->
  [(DriverRCAssociation, VehicleRegistrationCertificate)] ->
  m Common.DriverInfoRes
buildDriverInfoRes QPerson.DriverWithRidesCount {..} mbDriverLicense rcAssociationHistory = do
  mobileNumber <- traverse decrypt person.mobileNumber
  driverLicenseDetails <- traverse buildDriverLicenseAPIEntity mbDriverLicense
  vehicleRegistrationDetails <- traverse buildRCAssociationAPIEntity rcAssociationHistory
  availableMerchants <- case person.unencryptedMobileNumber of
    Just mbNumber -> do
      let mobileCountryCode = fromMaybe "+91" person.mobileCountryCode
      mobileNumberHash <- getDbHash mbNumber
      availablePersonWithNumber <- QPerson.findAllMerchantIdByPhoneNo mobileCountryCode mobileNumberHash
      let availableMerchantsId = map (.merchantId) availablePersonWithNumber
      availableMerchantsShortId <- CQM.findAllShortIdById availableMerchantsId
      pure $ map getShortId availableMerchantsShortId
    Nothing -> pure []
  pure
    Common.DriverInfoRes
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        numberOfRides = fromMaybe 0 ridesCount,
        mobileNumber,
        mobileCountryCode = person.mobileCountryCode,
        bundleVersion = person.bundleVersion,
        clientVersion = person.clientVersion,
        enabled = info.enabled,
        blocked = info.blocked,
        blockedReason = info.blockedReason,
        verified = info.verified,
        subscribed = info.subscribed,
        onboardingDate = info.lastEnabledOn,
        canDowngradeToSedan = info.canDowngradeToSedan,
        canDowngradeToHatchback = info.canDowngradeToHatchback,
        canDowngradeToTaxi = info.canDowngradeToTaxi,
        vehicleNumber = vehicle <&> (.registrationNo),
        driverLicenseDetails,
        vehicleRegistrationDetails,
        rating = person.rating,
        alternateNumber = person.unencryptedAlternateMobileNumber,
        availableMerchants = availableMerchants,
        blockStateModifier = info.blockStateModifier
      }

buildDriverLicenseAPIEntity :: EncFlow m r => DriverLicense -> m Common.DriverLicenseAPIEntity
buildDriverLicenseAPIEntity DriverLicense {..} = do
  licenseNumber' <- decrypt licenseNumber
  pure
    Common.DriverLicenseAPIEntity
      { driverLicenseId = cast @DriverLicense @Common.DriverLicense id,
        documentImageId1 = cast @Image @Common.Image documentImageId1,
        documentImageId2 = (cast @Image @Common.Image) <$> documentImageId2,
        licenseNumber = licenseNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        ..
      }

buildRCAssociationAPIEntity ::
  EncFlow m r =>
  (DriverRCAssociation, VehicleRegistrationCertificate) ->
  m Common.DriverRCAssociationAPIEntity
buildRCAssociationAPIEntity (DriverRCAssociation {..}, vehicleRC) = do
  details <- buildVehicleRCAPIEntity vehicleRC
  pure Common.DriverRCAssociationAPIEntity {..}

buildVehicleRCAPIEntity :: EncFlow m r => VehicleRegistrationCertificate -> m Common.VehicleRegistrationCertificateAPIEntity
buildVehicleRCAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  pure
    Common.VehicleRegistrationCertificateAPIEntity
      { registrationCertificateId = cast @VehicleRegistrationCertificate @Common.VehicleRegistrationCertificate id,
        documentImageId = cast @Image @Common.Image documentImageId,
        certificateNumber = certificateNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        ..
      }

castVerificationStatus :: IV.VerificationStatus -> Common.VerificationStatus
castVerificationStatus = \case
  IV.PENDING -> Common.PENDING
  IV.VALID -> Common.VALID
  IV.INVALID -> Common.INVALID

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId _ = DeleteDriver.deleteDriver merchantShortId . cast

---------------------------------------------------------------------
unlinkVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkVehicle merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  QDriverInfo.updateEnabledVerifiedState driverId False False
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  pure Success

---------------------------------------------------------------------
updatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  phoneNumberHash <- getDbHash req.newPhoneNumber
  mbLinkedPerson <- QPerson.findByMobileNumberAndMerchant req.newCountryCode phoneNumberHash merchant.id
  whenJust mbLinkedPerson $ \linkedPerson -> do
    if linkedPerson.id == driver.id
      then throwError $ InvalidRequest "Person already have the same mobile number"
      else throwError $ InvalidRequest "Person with this mobile number already exists"

  encNewPhoneNumber <- encrypt req.newPhoneNumber
  let updDriver =
        driver
          { DP.mobileCountryCode = Just req.newCountryCode,
            DP.mobileNumber = Just encNewPhoneNumber,
            DP.unencryptedMobileNumber = Just req.newPhoneNumber
          }
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  QPerson.updateMobileNumberAndCode updDriver
  QR.deleteByPersonId personId
  logTagInfo "dashboard -> updatePhoneNumber : " (show personId)
  pure Success

---------------------------------------------------------------------
addVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AddVehicleReq -> Flow APISuccess
addVehicle merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  mbLinkedVehicle <- QVehicle.findById personId
  whenJust mbLinkedVehicle $ \_ -> throwError VehicleAlreadyLinked

  let updDriver = driver {DP.firstName = req.driverName} :: DP.Person
  QPerson.updatePersonRec personId updDriver

  runVerifyRCFlow personId merchant merchantOpCityId req Nothing
  checkIfVehicleCreatedInRC <- QVehicle.findById personId
  unless (isJust checkIfVehicleCreatedInRC) $ do
    vehicle <- buildVehicle merchantId personId req
    -- Esq.runTransaction $ do
    QVehicle.create vehicle

  logTagInfo "dashboard -> addVehicle : " (show personId)
  pure Success

---------------------------------------------------------------------

addVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Text -> Common.AddVehicleReq -> Flow APISuccess
addVehicleForFleet merchantShortId opCity reqDriverPhoneNo mbMobileCountryCode fleetOwnerId req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash reqDriverPhoneNo
  let mobileCountryCode = fromMaybe mobileIndianCode mbMobileCountryCode
  driver <- QPerson.findByMobileNumberAndMerchant mobileCountryCode phoneNumberHash merchant.id >>= fromMaybeM (DriverNotFound reqDriverPhoneNo)
  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  vehicle <- RCQuery.findLastVehicleRCWrapper req.registrationNo
  whenJust vehicle $ \veh -> when (isJust veh.fleetOwnerId && veh.fleetOwnerId /= Just fleetOwnerId) $ throwError VehicleBelongsToAnotherFleet
  Redis.set (DomainRC.makeFleetOwnerKey req.registrationNo) fleetOwnerId -- setting this value here , so while creation of creation of vehicle we can add fleet owner id
  void $ runVerifyRCFlow driver.id merchant merchantOpCityId req (Just True)
  logTagInfo "dashboard -> addVehicle : " (show driver.id)
  pure Success

---------------------------------------------------------------------

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.RCStatusReq -> Flow APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity reqDriverId fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId
  when (isNothing isFleetDriver) $ throwError (InvalidRequest "Driver is not the  part of this fleet")
  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  vehicle <- RCQuery.findLastVehicleRCWrapper req.rcNo >>= fromMaybeM (VehicleDoesNotExist req.rcNo)
  unless (isJust vehicle.fleetOwnerId && vehicle.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  Redis.set (DomainRC.makeFleetOwnerKey req.rcNo) fleetOwnerId
  _ <- DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {isActivate = req.isActivate, rcNo = req.rcNo})
  logTagInfo "dashboard -> addVehicle : " (show driver.id)
  pure Success

---------------------------------------------------------------------

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation _merchantShortId _opCity fleetOwnerId mbLimit mbOffset = do
  let limit = min 25 $ fromMaybe 10 mbLimit -- TODO: we have to make this query more efficient
      offset = fromMaybe 0 mbOffset
  listOfAllDrivers <- FDV.findAllDriverByFleetOwnerId fleetOwnerId limit offset
  listOfAllVehicle <- RCQuery.findAllByFleetOwnerId' fleetOwnerId
  listItems <- createDriverVehicleAssociationListItem listOfAllDrivers listOfAllVehicle
  let filteredItems = filter (.isRcAssociated) listItems
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = filteredItems}
  where
    createDriverVehicleAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => [FleetDriverAssociation] -> [VehicleRegistrationCertificate] -> m [Common.DriveVehicleAssociationListItem]
    createDriverVehicleAssociationListItem fdaList vrcaList = do
      now <- getCurrentTime
      fmap concat $
        forM fdaList $ \fda -> do
          forM vrcaList $ \vrca -> do
            let driverId = fda.driverId
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            let driverName = Just driver.firstName
            driverInfo' <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
            decryptedVehicleRC <- decrypt vrca.certificateNumber
            rcAssociation <- QRCAssociation.findLinkedByRCIdAndDriverId driverId vrca.id now
            let vehicleType = castVehicleVariantDashboard vrca.vehicleVariant
            completedRides <- QRD.totalRidesByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) decryptedVehicleRC driverId
            earning <- QRide.totalEarningsByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) decryptedVehicleRC driverId
            let isDriverActive = fda.isActive
            let isRcAssociated = isJust rcAssociation
            let driverPhoneNo = driver.unencryptedMobileNumber
            let listItem =
                  Common.DriveVehicleAssociationListItem
                    { vehicleNo = Just decryptedVehicleRC,
                      status = Just $ castDriverStatus driverInfo'.mode,
                      driverId = Just driverId.getId,
                      ..
                    }
            pure listItem

getFleetDriverAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation _merchantShortId _opCity fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  listOfAllDrivers <- FDV.findAllDriverByFleetOwnerId fleetOwnerId limit offset
  listItems <- createFleetDriverAssociationListItem listOfAllDrivers
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems}
  where
    createFleetDriverAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => [FleetDriverAssociation] -> m [Common.DriveVehicleAssociationListItem]
    createFleetDriverAssociationListItem fdaList = do
      forM fdaList $ \fda -> do
        driver <- QPerson.findById fda.driverId >>= fromMaybeM (PersonNotFound fda.driverId.getId)
        driverActiveRcAssociation <- QRCAssociation.findActiveAssociationByDriver fda.driverId
        (vehicleNo, vehicleType) <- case driverActiveRcAssociation of
          Just rcAssociation -> do
            isAssociatedWithFleet <- RCQuery.findByRCIdAndFleetOwnerId rcAssociation.rcId fleetOwnerId
            case isAssociatedWithFleet of
              Just _ -> do
                decryptedVehicleRC <- RCQuery.findById rcAssociation.rcId >>= fromMaybeM (RCNotFound rcAssociation.rcId.getId)
                vehNo <- decrypt (decryptedVehicleRC.certificateNumber)
                pure (Just vehNo, castVehicleVariantDashboard decryptedVehicleRC.vehicleVariant)
              Nothing -> pure (Nothing, Nothing)
          Nothing -> pure (Nothing, Nothing)
        let driverName = Just driver.firstName
        let driverPhoneNo = driver.unencryptedMobileNumber
        driverInfo' <- QDriverInfo.findById fda.driverId >>= fromMaybeM DriverInfoNotFound
        completedRides <- QRD.totalRidesByFleetOwnerPerDriver (Just fleetOwnerId) driver.id
        earning <- QRide.totalEarningsByFleetOwnerPerDriver (Just fleetOwnerId) driver.id
        let driverStatus = if isNothing vehicleNo then Nothing else Just $ castDriverStatus driverInfo'.mode
        let isRcAssociated = isJust vehicleNo
        let isDriverActive = fda.isActive
        let driverId = Just $ fda.driverId.getId
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = vehicleNo,
                  status = driverStatus,
                  isDriverActive = isDriverActive,
                  ..
                }
        pure ls

getFleetVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.DrivertoVehicleAssociationRes
getFleetVehicleAssociation _merchantShortId _opCity fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  listOfAllVehicle <- RCQuery.findAllByFleetOwnerId fleetOwnerId limit offset
  listItems <- createFleetVehicleAssociationListItem listOfAllVehicle
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems}
  where
    createFleetVehicleAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => [VehicleRegistrationCertificate] -> m [Common.DriveVehicleAssociationListItem]
    createFleetVehicleAssociationListItem vrcList = do
      now <- getCurrentTime
      forM vrcList $ \vrc -> do
        decryptedVehicleRC <- decrypt vrc.certificateNumber
        completedRides <- QRD.totalRidesByFleetOwnerPerVehicle (Just fleetOwnerId) decryptedVehicleRC
        earning <- QRide.totalEarningsByFleetOwnerPerVehicle (Just fleetOwnerId) decryptedVehicleRC
        currentActiveAssociation <- QRCAssociation.findActiveAssociationByRC vrc.id
        (currentActiveDriver, status) <- case currentActiveAssociation of
          Just activeAssociation -> do
            driver <- QPerson.findById activeAssociation.driverId
            case driver of
              Just driver' -> do
                isDriverPartOfFleet <- FDV.findByDriverIdAndFleetOwnerId driver'.id fleetOwnerId
                case isDriverPartOfFleet of
                  Just _ -> do
                    driverInfo' <- QDriverInfo.findById driver'.id >>= fromMaybeM DriverInfoNotFound
                    pure (Just driver', driverInfo'.mode)
                  Nothing -> pure (Nothing, Nothing)
              Nothing -> pure (Nothing, Nothing)
          Nothing -> pure (Nothing, Nothing)

        (driverName, driverId, driverPhoneNo) <- case currentActiveDriver of
          Just driver -> pure (Just driver.firstName, Just driver.id.getId, driver.unencryptedMobileNumber)
          Nothing -> do
            latestAssociation <- QRCAssociation.findLatestLinkedByRCId vrc.id now
            case latestAssociation of
              Just latestAssociation' -> do
                driver <- QPerson.findById latestAssociation'.driverId
                case driver of
                  Just driver' -> pure (Just driver'.firstName, Just driver'.id.getId, driver'.unencryptedMobileNumber)
                  Nothing -> pure (Nothing, Nothing, Nothing)
              Nothing -> pure (Nothing, Nothing, Nothing)
        let vehicleType = castVehicleVariantDashboard vrc.vehicleVariant
        let isDriverActive = isJust currentActiveDriver -- Check if there is a current active driver
        let isRcAssociated = isJust currentActiveAssociation
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = Just decryptedVehicleRC,
                  status = Just $ castDriverStatus status,
                  isDriverActive = isDriverActive,
                  earning = earning,
                  completedRides = completedRides,
                  vehicleType = vehicleType,
                  ..
                }
        pure ls

castDriverStatus :: Maybe DrInfo.DriverMode -> Common.DriverMode
castDriverStatus = \case
  Just DrInfo.ONLINE -> Common.ONLINE
  Just DrInfo.OFFLINE -> Common.OFFLINE
  Just DrInfo.SILENT -> Common.SILENT
  Nothing -> Common.OFFLINE

---------------------------------------------------------------------

fleetUnlinkVehicle :: ShortId DM.Merchant -> Text -> Id Common.Driver -> Text -> Flow APISuccess
fleetUnlinkVehicle merchantShortId fleetOwnerId reqDriverId vehicleNo = do
  merchant <- findMerchantByShortId merchantShortId
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId
  case isFleetDriver of
    Nothing -> throwError (InvalidRequest "Driver is not part of this fleet, add this driver to the fleet before unlinking a vehicle with them")
    Just fleetDriver -> do
      unless fleetDriver.isActive $ throwError (InvalidRequest "Driver is not active with this fleet, add this driver to the fleet before unlinking  a vehicle linked with him")
  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  QDriverInfo.updateEnabledVerifiedState driverId False False
  rc <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (RCNotFound vehicleNo)
  _ <- QRCAssociation.endAssociationForRC personId rc.id
  logTagInfo "fleet -> unlinkVehicle : " (show personId)
  pure Success

runVerifyRCFlow :: Id DP.Person -> DM.Merchant -> Id DMOC.MerchantOperatingCity -> Common.AddVehicleReq -> Maybe Bool -> Flow ()
runVerifyRCFlow personId merchant merchantOpCityId req multipleRC = do
  let rcReq =
        DomainRC.DriverRCReq
          { vehicleRegistrationCertNumber = req.registrationNo,
            imageId = "",
            operatingCity = "Bangalore", -- TODO: this needs to be fixed properly
            dateOfRegistration = Nothing,
            multipleRC = multipleRC
          }
  void $ DomainRC.verifyRC True (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq (Just $ castVehicleVariant req.variant)

buildVehicle :: MonadFlow m => Id DM.Merchant -> Id DP.Person -> Common.AddVehicleReq -> m DVeh.Vehicle
buildVehicle merchantId personId req = do
  now <- getCurrentTime
  return $
    DVeh.Vehicle
      { driverId = personId,
        merchantId = merchantId,
        variant = castVehicleVariant req.variant,
        model = req.model,
        color = req.colour,
        vehicleName = Nothing,
        registrationNo = req.registrationNo,
        capacity = req.capacity,
        category = Nothing,
        make = req.make,
        size = Nothing,
        energyType = req.energyType,
        registrationCategory = Nothing,
        vehicleClass = req.vehicleClass,
        createdAt = now,
        updatedAt = now
      }

castVehicleVariant :: Common.Variant -> DVeh.Variant
castVehicleVariant = \case
  Common.SUV -> DVeh.SUV
  Common.HATCHBACK -> DVeh.HATCHBACK
  Common.SEDAN -> DVeh.SEDAN
  Common.AUTO_RICKSHAW -> DVeh.AUTO_RICKSHAW
  Common.TAXI -> DVeh.TAXI
  Common.TAXI_PLUS -> DVeh.TAXI_PLUS

castVehicleVariantDashboard :: Maybe DVeh.Variant -> Maybe Common.Variant
castVehicleVariantDashboard = \case
  Just DVeh.SUV -> Just Common.SUV
  Just DVeh.HATCHBACK -> Just Common.HATCHBACK
  Just DVeh.SEDAN -> Just Common.SEDAN
  Just DVeh.AUTO_RICKSHAW -> Just Common.AUTO_RICKSHAW
  Just DVeh.TAXI -> Just Common.TAXI
  Just DVeh.TAXI_PLUS -> Just Common.TAXI_PLUS
  _ -> Nothing

---------------------------------------------------------------------
getAllVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.ListVehicleRes
getAllVehicleForFleet _ _ fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  vehicleList <- RCQuery.findAllByFleetOwnerId fleetOwnerId limit offset
  vehicles <- traverse convertToVehicleAPIEntity vehicleList
  return $ Common.ListVehicleRes vehicles

convertToVehicleAPIEntity :: EncFlow m r => VehicleRegistrationCertificate -> m Common.VehicleAPIEntity
convertToVehicleAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  pure
    Common.VehicleAPIEntity
      { variant = castVehicleVariantDashboard vehicleVariant,
        model = vehicleModel,
        color = vehicleColor,
        registrationNo = certificateNumber'
      }

getAllDriverForFleet :: ShortId DM.Merchant -> Text -> Maybe Int -> Maybe Int -> Flow Common.FleetListDriverRes
getAllDriverForFleet _ fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  driverList <- FDV.findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset
  let driverIdList :: [Id DP.Person] = map DTFDA.driverId driverList
  driversInfo <- QPerson.getDriversByIdIn driverIdList
  let fleetDriversInfos = map convertToDriverAPIEntity driversInfo
  return $ Common.FleetListDriverRes fleetDriversInfos

convertToDriverAPIEntity :: DP.Person -> Common.FleetDriversAPIEntity
convertToDriverAPIEntity DP.Person {..} =
  Common.FleetDriversAPIEntity
    { driverId = cast @DP.Person @Common.Driver id,
      firstName = firstName,
      middleName = middleName,
      lastName = lastName,
      mobileNumber = unencryptedMobileNumber,
      mobileCountryCode = mobileCountryCode
    }

---------------------------------------------------------------------
updateDriverName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
updateDriverName merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateUpdateDriverNameReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  -- empty string in request condsidered as Nothing in db, Nothing in request is not affect db value
  let updDriver =
        driver{firstName = req.firstName,
               middleName = if req.middleName == Just "" then Nothing else req.middleName <|> driver.middleName,
               lastName = if req.lastName == Just "" then Nothing else req.lastName <|> driver.lastName
              }

  QPerson.updatePersonRec personId updDriver

  logTagInfo "dashboard -> updateDriverName : " (show personId)
  pure Success

---------------------------------------------------------------------
unlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkDL merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  QDriverLicense.deleteByDriverId personId
  QDriverInfo.updateEnabledVerifiedState driverId_ False False
  logTagInfo "dashboard -> unlinkDL : " (show personId)
  pure Success

---------------------------------------------------------------------
unlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkAadhaar merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  _ <- B.runInReplica $ AV.findByDriverId personId >>= fromMaybeM (InvalidRequest "can't unlink Aadhaar")

  AV.deleteByDriverId personId
  QDriverInfo.updateAadhaarVerifiedState driverId_ False
  unless (transporterConfig.aadhaarVerificationRequired) $ QDriverInfo.updateEnabledVerifiedState driverId_ False False
  logTagInfo "dashboard -> unlinkAadhaar : " (show personId)
  pure Success

---------------------------------------------------------------------
endRCAssociation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
endRCAssociation merchantShortId opCity reqDriverId = do
  -- API should be deprecated
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId

  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  associations <- QRCAssociation.findAllLinkedByDriverId personId
  mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
  let mVehicleRC = listToMaybe (catMaybes mVehicleRCs)

  case mVehicleRC of
    Just vehicleRC -> do
      rcNo <- decrypt vehicleRC.certificateNumber
      void $ DomainRC.deleteRC (personId, merchant.id, merchantOpCityId) (DomainRC.DeleteRCReq {rcNo}) True
    Nothing -> throwError (InvalidRequest "No linked RC  to driver")

  QDriverInfo.updateEnabledVerifiedState driverId False False
  logTagInfo "dashboard -> endRCAssociation : " (show personId)
  pure Success

setRCStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RCStatusReq -> Flow APISuccess
setRCStatus merchantShortId opCity reqDriverId Common.RCStatusReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {..})

deleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> Flow APISuccess
deleteRC merchantShortId opCity reqDriverId Common.DeleteRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  DomainRC.deleteRC (personId, merchant.id, merchantOpCityId) (DomainRC.DeleteRCReq {..}) False

getPaymentHistory :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Flow Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getDriverPaymentsHistoryV2 (personId, merchant.id, merchantOpCityId) invoicePaymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Id INV.Invoice -> Flow Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getHistoryEntryDetailsEntityV2 (personId, merchant.id, merchantOpCityId) invoiceId.getId

updateSubscriptionDriverFeeAndInvoice :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.SubscriptionDriverFeesAndInvoicesToUpdate -> Flow Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId Common.SubscriptionDriverFeesAndInvoicesToUpdate {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  maybe (pure ()) (`QDriverInfo.updateSubscription` personId) subscribed
  dueDriverFee <- QDF.findAllPendingAndDueDriverFeeByDriverId personId
  let invoicesDataToUpdate =
        maybe
          []
          ( map
              ( \invData -> do
                  let mbInvoiceStatus = (\invs -> readMaybe (T.unpack invs) :: (Maybe INV.InvoiceStatus)) =<< invData.invoiceStatus
                  InvoiceInfoToUpdateAfterParse
                    { invoiceId = cast (Id invData.invoiceId),
                      driverFeeId = cast . Id <$> invData.driverFeeId,
                      invoiceStatus = mbInvoiceStatus
                    }
              )
          )
          invoices
  mapM_ (\inv -> QINV.updateStatusAndTypeByMbdriverFeeIdAndInvoiceId inv.invoiceId inv.invoiceStatus Nothing inv.driverFeeId) invoicesDataToUpdate
  allDriverFeeByIds <- QDF.findAllByDriverFeeIds (maybe [] (map (\df -> cast (Id df.driverFeeId))) driverFees)
  if isJust mkDuesToAmount
    then do
      let amount = maybe 0 (/ (fromIntegral $ length dueDriverFee)) mkDuesToAmount
      mapM_ (\feeId -> QDF.resetFee feeId 0 amount 0 0 Nothing now) (dueDriverFee <&> (.id))
      return $ mkResponse dueDriverFee
    else do
      maybe
        (pure ())
        ( mapM_
            ( \fee -> do
                let id = cast (Id fee.driverFeeId)
                    platFormFee = fromMaybe 0 (fee.platformFee)
                    sgst = fromMaybe 0 (fee.sgst)
                    cgst = fromMaybe 0 (fee.cgst)
                QDF.resetFee id 0 platFormFee sgst cgst Nothing now
                when (fee.mkManualDue == Just True) $ do QDF.updateAutoPayToManual id
                when (fee.mkAutoPayDue == Just True && fee.mkManualDue `elem` [Nothing, Just False]) $ do QDF.updateManualToAutoPay id
            )
        )
        driverFees
      return $ mkResponse allDriverFeeByIds
  where
    mkResponse driverFees' =
      Common.SubscriptionDriverFeesAndInvoicesToUpdate
        { driverFees =
            Just $
              map
                ( \dfee ->
                    Common.DriverFeeInfoToUpdate
                      { driverFeeId = dfee.id.getId,
                        mkManualDue = Nothing,
                        mkAutoPayDue = Nothing,
                        mkCleared = Nothing,
                        platformFee = Just $ dfee.platformFee.fee,
                        cgst = Just dfee.platformFee.cgst,
                        sgst = Just dfee.platformFee.sgst
                      }
                )
                driverFees',
          invoices = Nothing,
          mkDuesToAmount = Nothing,
          subscribed = Nothing
        }

data InvoiceInfoToUpdateAfterParse = InvoiceInfoToUpdateAfterParse
  { invoiceId :: Id INV.Invoice,
    driverFeeId :: Maybe (Id DriverFee),
    invoiceStatus :: Maybe INV.InvoiceStatus
  }

---------------------------------------------------------------------
clearOnRideStuckDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Flow Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId _ dbSyncTime = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  let dbSyncInterVal = addUTCTime (fromIntegral (- fromMaybe 1 dbSyncTime) * 60) now
  driverInfosAndRideDetails <- B.runInReplica $ QPerson.getOnRideStuckDriverIds dbSyncInterVal
  driverIds <-
    mapM
      ( \dI -> do
          QDI.updateOnRide (cast dI.driverInfo.driverId) False
          void $ LF.rideDetails dI.ride.id SRide.CANCELLED merchant.id dI.ride.driverId dI.ride.fromLocation.lat dI.ride.fromLocation.lon
          return (cast dI.driverInfo.driverId)
      )
      driverInfosAndRideDetails

  return Common.ClearOnRideStuckDriversRes {driverIds = driverIds}

---------------------------------------------------------------------
getDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  dghLocs <- DDriver.getHomeLocations (cast driverId, cast merchant.id, merchantOpCityId)
  return (buildDriverHomeLocationAPIEntity <$> dghLocs.locations)
  where
    buildDriverHomeLocationAPIEntity dghLocs =
      Common.DriverHomeLocationAPIEntity
        { id = cast dghLocs.id,
          address = dghLocs.address,
          lat = dghLocs.lat,
          lon = dghLocs.lon,
          tag = dghLocs.tag
        }

updateDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> Flow APISuccess
updateDriverHomeLocation _ _ _ req = do
  QDHL.updateHomeLocationById (cast req.id) buildDriverHomeLocationEntity
  return Success
  where
    buildDriverHomeLocationEntity =
      DDHL.UpdateDriverHomeLocation
        { address = req.address,
          lat = req.lat,
          lon = req.lon,
          tag = req.tag
        }

incrementDriverGoToCount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
incrementDriverGoToCount merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  CQDGR.increaseDriverGoHomeRequestCount merchantOpCityId (cast driverId)
  return Success

getDriverGoHomeInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.CachedGoHomeRequestInfoRes
getDriverGoHomeInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  ghInfo <- CQGHC.getDriverGoHomeRequestInfo (cast driverId) merchantOpCityId Nothing
  return (buildCachedGoHomeRequestInfoRes ghInfo)
  where
    buildCachedGoHomeRequestInfoRes CachedGoHomeRequest {..} =
      Common.CachedGoHomeRequestInfoRes
        { status = show <$> status,
          driverGoHomeRequestId = cast <$> driverGoHomeRequestId,
          ..
        }

---------------------------------------------------------------------
driverAadhaarInfoByPhone :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId _ phoneNumber = do
  merchant <- findMerchantByShortId merchantShortId
  mobileNumberHash <- getDbHash phoneNumber
  driver <- QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash merchant.id >>= fromMaybeM (InvalidRequest "Person not found")
  res <- AV.findByDriverId driver.id
  case res of
    Just aadhaarData -> do
      pure
        Common.DriverAadhaarInfoRes
          { driverName = aadhaarData.driverName,
            driverGender = aadhaarData.driverGender,
            driverDob = aadhaarData.driverDob,
            driverImage = aadhaarData.driverImage
          }
    Nothing -> throwError $ InvalidRequest "no aadhaar data is found"

---------------------------------------------------------------------

---------------------------------------------------------------------
updateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> Flow APISuccess
updateByPhoneNumber merchantShortId _ phoneNumber req = do
  mobileNumberHash <- getDbHash phoneNumber
  aadhaarNumberHash <- getDbHash req.driverAadhaarNumber
  aadhaarInfo <- AV.findByAadhaarNumberHash aadhaarNumberHash
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
  merchant <- findMerchantByShortId merchantShortId
  driver <- QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash merchant.id >>= fromMaybeM (InvalidRequest "Person not found")
  res <- AV.findByDriverId driver.id
  case res of
    Just _ -> AV.findByPhoneNumberAndUpdate req.driverName req.driverGender req.driverDob (Just aadhaarNumberHash) req.isVerified driver.id
    Nothing -> do
      aadhaarEntity <- AVD.mkAadhaar driver.id req.driverName req.driverGender req.driverDob (Just aadhaarNumberHash) Nothing True Nothing
      AV.create aadhaarEntity
  QDriverInfo.updateAadhaarVerifiedState (cast driver.id) True
  pure Success

fleetRemoveVehicle :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow APISuccess
fleetRemoveVehicle _merchantShortId _ fleetOwnerId_ vehicleNo = do
  vehicle <- QVehicle.findByRegistrationNo vehicleNo
  whenJust vehicle $ \veh -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId veh.driverId fleetOwnerId_
    when (isJust isFleetDriver) $ throwError (InvalidRequest "Vehcile is linked to fleet driver , first unlink then try")
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (VehicleDoesNotExist vehicleNo)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId_) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId_)
  RCQuery.upsert (updatedVehicleRegistrationCertificate vehicleRC)
  pure Success
  where
    updatedVehicleRegistrationCertificate VehicleRegistrationCertificate {..} = VehicleRegistrationCertificate {fleetOwnerId = Nothing, ..}

fleetRemoveDriver :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow APISuccess
fleetRemoveDriver _merchantShortId _ fleetOwnerId driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  associationList <- QRCAssociation.findAllLinkedByDriverId personId
  fleetOwnerRcList <- RCQuery.findAllByFleetOwnerId' fleetOwnerId
  let rcAssociatedList :: [Id VehicleRegistrationCertificate] = map (.id) fleetOwnerRcList
  let rcAssociatedWithFleet = any (\rcAssoc -> rcAssoc.rcId `elem` rcAssociatedList) associationList
  when rcAssociatedWithFleet $ throwError (InvalidRequest "Driver is linked to fleet Vehicle , first unlink then try")
  FDV.updateFleetDriverActiveStatus fleetOwnerId personId False
  pure Success

fleetTotalEarning :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.FleetTotalEarningResponse -- TODO: This is thing should be in interval level this will become very slow when the data will grow
fleetTotalEarning _merchantShortId _ fleetOwnerId = do
  totalRides <- QRD.totalRidesByFleetOwner (Just fleetOwnerId)
  totalCompletedRides <- QRD.totalRidesCompletedInFleet (Just fleetOwnerId)
  totalCancelledRides <- QRD.totalRidesCancelledInFleet (Just fleetOwnerId)
  totalEarning <- QRide.totalEarningsByFleetOwner (Just fleetOwnerId)
  allVehicle <- RCQuery.findAllByFleetOwnerId' fleetOwnerId
  let totalVehicle = length allVehicle
  let conversionRate = (fromIntegral (totalCompletedRides * 100) :: Double) / (fromIntegral (nonZero totalRides) :: Double)
  let cancellationRate = (fromIntegral (totalCancelledRides * 100) :: Double) / (fromIntegral (nonZero totalRides) :: Double)
  pure $ Common.FleetTotalEarningResponse {..}

nonZero :: Int -> Int
nonZero a
  | a <= 0 = 1
  | otherwise = a

fleetVehicleEarning :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Maybe (Id Common.Driver) -> Flow Common.FleetEarningRes
fleetVehicleEarning _merchantShortId _ fleetOwnerId vehicleNo mbDriverId = do
  case mbDriverId of
    Just driverId -> fleetVehicleEarningPerDriver fleetOwnerId vehicleNo driverId
    Nothing -> do
      totalRides <- QRD.totalRidesByFleetOwnerPerVehicle (Just fleetOwnerId) vehicleNo
      totalEarning <- QRide.totalEarningsByFleetOwnerPerVehicle (Just fleetOwnerId) vehicleNo
      vehicleRegCert <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (VehicleNotFound vehicleNo)
      currentActiveAssociation <- QRCAssociation.findActiveAssociationByRC vehicleRegCert.id
      currentActiveDriver <- case currentActiveAssociation of
        Just activeAssociation -> do
          driver <- QPerson.findById activeAssociation.driverId >>= fromMaybeM (PersonNotFound activeAssociation.driverId.getId)
          pure $ Just driver
        Nothing -> pure Nothing
      (dId, driverName) <- case currentActiveDriver of
        Just driver -> pure (Just $ cast @DP.Person @Common.Driver driver.id, Just $ driver.firstName <> " " <> fromMaybe "" driver.lastName)
        Nothing -> pure (Nothing, Nothing)
      let vehicleType = castVehicleVariantDashboard (vehicleRegCert.vehicleVariant)
      pure $
        Common.FleetEarningRes
          { driverId = dId,
            driverName = driverName,
            vehicleNo = Just vehicleNo,
            status = Nothing,
            ..
          }

fleetVehicleEarningPerDriver :: Text -> Text -> Id Common.Driver -> Flow Common.FleetEarningRes
fleetVehicleEarningPerDriver fleetOwnerId vehicleNo driverId = do
  let dId = cast @Common.Driver @DP.Person driverId
  driver <- QPerson.findById dId >>= fromMaybeM (PersonNotFound dId.getId)
  totalRides <- QRD.totalRidesByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) vehicleNo dId
  totalEarning <- QRide.totalEarningsByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) vehicleNo driver.id
  let driverName = driver.firstName <> " " <> fromMaybe "" driver.lastName
  vehicleRegCert <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (VehicleNotFound vehicleNo)
  let vehicleType = castVehicleVariantDashboard vehicleRegCert.vehicleVariant
  driverInfo' <- QDriverInfo.findById (cast dId) >>= fromMaybeM DriverInfoNotFound
  pure $
    Common.FleetEarningRes
      { driverId = Just driverId,
        driverName = Just driverName,
        totalRides = totalRides,
        totalEarning = totalEarning,
        vehicleNo = Just vehicleNo,
        status = Just $ castDriverStatus driverInfo'.mode,
        ..
      }

fleetDriverEarning :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow Common.FleetEarningRes
fleetDriverEarning _merchantShortId _ fleetOwnerId driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverActiveRcAssociation <- QRCAssociation.findActiveAssociationByDriver personId
  (vehicleNo, variant) <- case driverActiveRcAssociation of
    Just rcAssociation -> do
      vehicleRC <- RCQuery.findById rcAssociation.rcId >>= fromMaybeM (RCNotFound rcAssociation.rcId.getId)
      vehNo <- decrypt vehicleRC.certificateNumber
      pure (vehNo, vehicleRC.vehicleVariant)
    Nothing -> pure ("", Nothing)
  let driverName = driver.firstName <> " " <> fromMaybe "" driver.lastName
  totalRides <- QRD.totalRidesByFleetOwnerPerDriver (Just fleetOwnerId) personId
  totalEarning <- QRide.totalEarningsByFleetOwnerPerDriver (Just fleetOwnerId) driver.id
  driverInfo' <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
  pure $
    Common.FleetEarningRes
      { driverId = Just driverId,
        driverName = Just driverName,
        totalRides = totalRides,
        totalEarning = totalEarning,
        vehicleNo = Just vehicleNo,
        status = Just $ castDriverStatus driverInfo'.mode,
        vehicleType = castVehicleVariantDashboard variant,
        ..
      }

------------------------------------------------------------------------------------------------
data MediaChannel
  = SMS
  | WHATSAPP
  | OVERLAY
  | ALERT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data SendSmsReq = SendSmsReq
  { channel :: MediaChannel,
    messageKey :: MessageKey,
    overlayKey :: Maybe Text,
    messageId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets SendSmsReq where
  hideSecrets = identity

data VolunteerTransactionStorageReq = VolunteerTransactionStorageReq
  { volunteerId :: Text,
    driverId :: Text,
    messageKey :: Text,
    channel :: Text,
    overlayKey :: Text,
    messageId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets VolunteerTransactionStorageReq where
  hideSecrets = identity

sendSmsToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> SendSmsReq -> Flow APISuccess
sendSmsToDriver merchantShortId opCity driverId volunteerId _req@SendSmsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- limit checking
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  void $ checkIfVolunteerSMSSendingLimitExceeded volunteerId transporterConfig.volunteerSmsSendingLimit channel
  void $ checkIfDriverSMSReceivingLimitExceeded driverId.getId transporterConfig.driverSmsReceivingLimit channel

  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access check
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> personId.getId) $ do
    case channel of
      SMS -> do
        message <- MessageBuilder.buildGenericMessage merchantOpCityId messageKey MessageBuilder.BuildGenericMessageReq {}
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKey merchantOpCityId messageKey
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
        let jsonData = merchantMessage.jsonData
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId jsonData.var1 jsonData.var2 jsonData.var3 (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      OVERLAY -> whenJust overlayKey $ \oKey -> do
        manualDues <- getManualDues personId transporterConfig.timeDiffFromUtc transporterConfig.driverFeeOverlaySendingTimeLimitInDays
        mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf merchantOpCityId oKey (fromMaybe ENGLISH driver.language) Nothing
        whenJust mOverlay $ \overlay -> do
          let okButtonText = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.okButtonText
          let description = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.description
          TN.sendOverlay merchantOpCityId driver.id driver.deviceToken overlay.title description overlay.imageUrl okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks
      ALERT -> whenJust messageId $ \_mId -> do
        topicName <- asks (.broadcastMessageTopic)
        message <- B.runInReplica $ MQuery.findById (Id _mId) >>= fromMaybeM (InvalidRequest "Message Not Found")
        msg <- createMessageLanguageDict message
        produceMessage (topicName, Just (encodeUtf8 $ getId driverId)) msg
  -- if the message is sent successfuly then increment the count of both volunteer and driver
  void $ incrementVolunteerSMSSendingCount volunteerId channel
  void $ incrementDriverSMSReceivingCount driverId.getId channel
  pure Success
  where
    createMessageLanguageDict :: Domain.RawMessage -> Flow Domain.MessageDict
    createMessageLanguageDict message = do
      translations <- B.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

    addTranslation Domain.RawMessage {..} trans =
      (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

    getManualDues personId timeDiffFromUtc driverFeeOverlaySendingTimeLimitInDays = do
      windowEndTime <- getLocalCurrentTime timeDiffFromUtc
      let windowStartTime = addUTCTime (-1 * fromIntegral driverFeeOverlaySendingTimeLimitInDays * 86400) (UTCTime (utctDay windowEndTime) (secondsToDiffTime 0))
      pendingDriverFees <- QDF.findAllOverdueDriverFeeByDriverId personId
      let filteredDriverFees = filter (\driverFee -> driverFee.startTime >= windowStartTime) pendingDriverFees
      return $
        if null filteredDriverFees
          then 0
          else sum $ map (\dueInvoice -> SLDriverFee.roundToHalf (fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

    templateText txt = "{#" <> txt <> "#}"

windowLimit :: SWC.SlidingWindowOptions
windowLimit = SWC.SlidingWindowOptions 24 SWC.Hours

checkIfVolunteerSMSSendingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfVolunteerSMSSendingLimitExceeded volunteerId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 500
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit
  when (currentLimit >= limit) $ throwError VolunteerSmsSendingLimitExceeded -- the limit is counted from 0

incrementVolunteerSMSSendingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementVolunteerSMSSendingCount volunteerId channel = SWC.incrementWindowCount (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit

checkIfDriverSMSReceivingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfDriverSMSReceivingLimitExceeded driverId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 10
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkDriverSMSRecevingLimitKey driverId channel) windowLimit
  when (currentLimit >= limit) $ throwError DriverSmsReceivingLimitExceeded -- the limit is counted from 0

incrementDriverSMSReceivingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementDriverSMSReceivingCount driverId channel = SWC.incrementWindowCount (mkDriverSMSRecevingLimitKey driverId channel) windowLimit

mkVolunteerSMSSendingLimitKey :: Text -> MediaChannel -> Text
mkVolunteerSMSSendingLimitKey volunteerId channel = "Dashboard:VolunteerId-" <> volunteerId <> ":channel-" <> show channel <> ":SendSMS:HitCount"

mkDriverSMSRecevingLimitKey :: Text -> MediaChannel -> Text
mkDriverSMSRecevingLimitKey driverId channel = "Dashboard:DriverId-" <> driverId <> ":channel-" <> show channel <> ":SendSMS:ReceiveCount"

getLimitAccordingToChannel :: DashboardMediaSendingLimit -> MediaChannel -> Int
getLimitAccordingToChannel config channel =
  case channel of
    SMS -> config.sms
    WHATSAPP -> config.whatsapp
    OVERLAY -> config.overlay
    ALERT -> config.alert

--------------------------------------------------------------------------------------------------
