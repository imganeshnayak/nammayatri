{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person
  ( module Storage.Queries.Person,
    module Reexport,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import qualified Database.Beam as B
import Database.Beam.Postgres hiding ((++.))
import qualified Database.Beam.Query ()
import qualified Domain.Types.Booking as Booking
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import qualified Domain.Types.DriverLocation as DDL
import Domain.Types.DriverQuote as DriverQuote
import Domain.Types.Merchant hiding (MerchantAPIEntity)
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle as DV
import qualified EulerHS.Language as L
import IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import qualified Sequelize as Se
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride.Table as BeamR
import qualified Storage.Beam.Vehicle as BeamV
import Storage.Queries.Booking ()
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverOnboarding.DriverLicense ()
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation ()
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()
import Storage.Queries.DriverQuote ()
import Storage.Queries.Instances.DriverInformation ()
import Storage.Queries.Instances.Person ()
import Storage.Queries.Person.GetNearestDrivers as Reexport
import Storage.Queries.Person.GetNearestDriversCurrentlyOnRide as Reexport
import Storage.Queries.Person.GetNearestGoHomeDrivers as Reexport
import qualified Storage.Queries.Person.Internal as Int
import Storage.Queries.Ride ()
import Storage.Queries.Vehicle ()
import qualified Storage.Queries.Vehicle.Internal as Int

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Person.Person -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe Person)
findById (Id personId) = findOneWithKV [Se.Is BeamP.id $ Se.Eq personId]

findAllDriversWithInfoAndVehicle ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId moCityId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString = do
  dbConf <- getMasterBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(person, driverInfo, vehicle) ->
                  person.merchantId B.==?. B.val_ (getId merchantId)
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\vehNum -> B.maybe_ (B.sqlBool_ $ B.val_ False) (\rNo -> B.sqlBool_ (B.like_ rNo (B.val_ ("%" <> vehNum <> "%")))) vehicle.registrationNo) mbVehicleNumberSearchString
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\verified -> driverInfo.verified B.==?. B.val_ verified) mbVerified
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\enabled -> driverInfo.enabled B.==?. B.val_ enabled) mbEnabled
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\blocked -> driverInfo.blocked B.==?. B.val_ blocked) mbBlocked
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\subscribed -> driverInfo.subscribed B.==?. B.val_ subscribed) mbSubscribed
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ (Just searchStrDBHash)) mbSearchPhoneDBHash
              )
              do
                person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
                driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
                vehicle <- B.leftJoin_' (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\veh' -> BeamP.id person B.==?. BeamV.driverId veh')
                pure (person, driverInfo, vehicle)
  case result of
    Right x -> do
      let persons = fmap fst' x
          driverInfos = fmap snd' x
          vehicles = fmap thd' x
      p <- catMaybes <$> mapM fromTType' persons
      di <- catMaybes <$> mapM fromTType' driverInfos
      v <- mapM (maybe (pure Nothing) fromTType') vehicles
      pure $ filter (\(person, _, _) -> person.merchantOperatingCityId == moCityId) $ zip3 p di v
    Left _ -> pure []
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z

getDriversList ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverInformation] ->
  m [Person]
getDriversList driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personsKeys]
  where
    personsKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriversByIdIn :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person] -> m [Person]
getDriversByIdIn personIds = findAllWithKV [Se.Is BeamP.id $ Se.In $ getId <$> personIds]

getDriverInformations ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInformations driverLocations =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamDI.active $ Se.Eq True]
            <> [Se.Is BeamDI.driverId $ Se.In personKeys]
        )
    ]
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocations

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }
  deriving (Generic)

findAllDriversByIdsFirstNameAsc ::
  (Functor m, MonadFlow m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc _merchantId driverIds = do
  driverLocs <- LF.driversLocation driverIds
  driverInfos <- Int.getDriverInfos $ map ((.getId) . DDL.driverId) driverLocs
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  return (linkArrays driverLocs driverInfos vehicle drivers)
  where
    linkArrays driverLocations driverInformations vehicles persons =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverInfoHashMap = HashMap.fromList $ (\di -> (di.driverId, di)) <$> driverInformations
       in mapMaybe (buildFullDriver personHashMap vehicleHashMap driverInfoHashMap) driverLocations

    buildFullDriver personHashMap vehicleHashMap driverInfoHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      Just $ FullDriver person location info vehicle

data DriverInfosAndRideDetails = DriverInfosAndRideDetails
  { driverInfo :: DriverInformation,
    ride :: Ride.Ride
  }

getOnRideStuckDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => UTCTime -> m [DriverInfosAndRideDetails]
getOnRideStuckDriverIds dbSyncInterVal = do
  rideDetails <- findAllWithDb [Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]
  let driverIds = Ride.driverId <$> rideDetails
  driverInfos <- findAllWithDb [Se.And [Se.Is BeamDI.onRide $ Se.Eq True, Se.Is BeamDI.updatedAt $ Se.LessThanOrEq dbSyncInterVal, Se.Is BeamDI.driverId $ Se.Not $ Se.In (getId <$> driverIds)]]
  return (linkArrays driverInfos rideDetails driverIds)
  where
    linkArrays driverInfos rideDetails driverIds =
      let driverInfosHashMap = HashMap.fromList $ (\p -> (p.driverId, p)) <$> driverInfos
          rideDetailsHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> rideDetails
       in mapMaybe (buildDriverInfosAndRideDetails driverInfosHashMap rideDetailsHashMap) driverIds

    buildDriverInfosAndRideDetails driverInfosHashMap rideDetailsHashMap driverId = do
      driverInfo <- HashMap.lookup driverId driverInfosHashMap
      rideDetail <- HashMap.lookup driverId rideDetailsHashMap
      Just $ DriverInfosAndRideDetails driverInfo rideDetail

fetchDriverIDsFromDriverQuotes :: [DriverQuote] -> [Id Person]
fetchDriverIDsFromDriverQuotes = map DriverQuote.driverId

fetchQuoteIdFromDriverQuotes :: [DriverQuote] -> [Text]
fetchQuoteIdFromDriverQuotes = map (.id.getId)

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

findAllDriverInformationWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => [Se.Clause Postgres BeamDI.DriverInformationT] -> m [DriverInformation]
findAllDriverInformationWithSeConditions = findAllWithKV

findAllVehiclesWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamV.VehicleT] -> m [Vehicle]
findAllVehiclesWithSeConditions = findAllWithKV

findAllBookingsWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamB.BookingT] -> m [Booking.Booking]
findAllBookingsWithSeConditions = findAllWithKV

findAllDriverQuoteWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamDQ.DriverQuoteT] -> m [DriverQuote]
findAllDriverQuoteWithSeConditions = findAllWithKV

findAllPersonWithSeConditionsNameAsc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditionsNameAsc conditions = findAllWithOptionsKV conditions (Se.Asc BeamP.firstName) Nothing Nothing

findAllPersonWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditions = findAllWithKV

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

fetchDriverInfoWithRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  mbDriverInfo <- fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash
  addRidesCount `mapM` mbDriverInfo
  where
    addRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Person, DriverInformation, Maybe Vehicle) -> m DriverWithRidesCount
    addRidesCount (person, info, vehicle) = do
      dbConf <- getMasterBeamConfig
      resp <-
        L.runDB dbConf $
          L.findRow $
            B.select $
              B.aggregate_ (\ride -> (B.group_ (BeamR.driverId ride), B.as_ @Int B.countAll_)) $
                B.filter_' (\(BeamR.RideT {driverId, status}) -> driverId B.==?. B.val_ (getId person.id) B.&&?. B.sqlNot_ (B.sqlBool_ (B.in_ status $ B.val_ <$> [Ride.NEW, Ride.CANCELLED]))) $
                  B.all_ (BeamCommon.ride BeamCommon.atlasDB)
      let ridesCount = either (const (Just 0)) (snd <$>) resp
      pure (mkDriverWithRidesCount (person, info, vehicle, ridesCount))

fetchDriverInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe (Person, DriverInformation, Maybe Vehicle))
fetchDriverInfo (Id merchantId) mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  dbConf <- getMasterBeamConfig
  now <- getCurrentTime
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(person, _driverInfo, vehicle, driverLicense, _driverRCAssociation, vehicleRegistrationCertificate) ->
              person.merchantId B.==?. B.val_ merchantId
                B.&&?. person.role B.==?. B.val_ Person.DRIVER
                B.&&?. maybe
                  (B.sqlBool_ $ B.val_ True)
                  ( \(mobileNumberDbHash, mobileCountryCode) ->
                      person.mobileCountryCode B.==?. B.val_ (Just mobileCountryCode)
                        B.&&?. (person.mobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash) B.||?. person.alternateMobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash))
                  )
                  mbMobileNumberDbHashWithCode
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\vehicleNo -> vehicle.registrationNo B.==?. B.val_ (Just vehicleNo)) mbVehicleNumber
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\dlNumberHash -> driverLicense.licenseNumberHash B.==?. B.val_ (Just dlNumberHash)) mbDlNumberHash
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rcNumberHash -> vehicleRegistrationCertificate.certificateNumberHash B.==?. B.val_ (Just rcNumberHash)) mbRcNumberHash
          )
          do
            person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
            driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
            vehicle <- B.leftJoin_' (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\veh' -> BeamP.id person B.==?. BeamV.driverId veh')
            driverLicense <- B.leftJoin_' (B.all_ (BeamCommon.driverLicense BeamCommon.atlasDB)) (\dl' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamP.id person B.==?. BeamDL.driverId dl') mbDlNumberHash)
            driverRCAssociation <- B.leftJoin_' (B.all_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB)) (\drca' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamP.id person B.==?. BeamDRCA.driverId drca' B.&&?. B.sqlBool_ (B.just_ (B.val_ now) B.<. BeamDRCA.associatedTill drca')) mbRcNumberHash)
            vehicleRegistrationCertificate <- B.leftJoin_' (B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)) (\vrc' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamDRCA.rcId driverRCAssociation B.==?. B.just_ (BeamVRC.id vrc')) mbRcNumberHash)
            pure (person, driverInfo, vehicle, driverLicense, driverRCAssociation, vehicleRegistrationCertificate)
  res' <- case result of
    Right x -> do
      let persons = fmap fst' x
          driverInfos = fmap snd' x
          vehicles = fmap thd' x
      p <- catMaybes <$> mapM fromTType' persons
      di <- catMaybes <$> mapM fromTType' driverInfos
      v <- mapM (maybe (pure Nothing) fromTType') vehicles
      pure $ zip3 p di v
    Left _ -> pure []
  pure $ listToMaybe res'
  where
    fst' (x, _, _, _, _, _) = x
    snd' (_, x, _, _, _, _) = x
    thd' (_, _, x, _, _, _) = x

findByIdAndRoleAndMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Person.Role -> Id Merchant -> m (Maybe Person)
findByIdAndRoleAndMerchantId (Id pid) role_ (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.id $ Se.Eq pid, Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findAllByMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantId roles (Id merchantId) = findAllWithDb [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]

findAdminsByMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = findAllWithDb [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]

findByMobileNumberAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash (Id merchantId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.merchantId $ Se.Eq merchantId,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
        ]
    ]

findByIdentifierAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Text -> m (Maybe Person)
findByIdentifierAndMerchant (Id merchantId) identifier_ = findOneWithKV [Se.And [Se.Is BeamP.identifier $ Se.Eq $ Just identifier_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByEmailAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Text -> m (Maybe Person)
findByEmailAndMerchant (Id merchantId) email_ = findOneWithKV [Se.And [Se.Is BeamP.email $ Se.Eq $ Just email_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByRoleAndMobileNumberAndMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber (Id merchantId) = do
  mobileNumberDbHash <- getDbHash mobileNumber
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.role $ Se.Eq role_,
          Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberDbHash,
          Se.Is BeamP.merchantId $ Se.Eq merchantId
        ]
    ]

findAllDriverIdExceptProvided :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided (Id merchantId) moCityId driverIdsToBeExcluded = do
  dbConf <- getMasterBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(person, driverInfo) ->
              person.merchantId B.==?. B.val_ merchantId
                B.&&?. driverInfo.verified B.==?. B.val_ True
                B.&&?. driverInfo.enabled B.==?. B.val_ True
                B.&&?. driverInfo.blocked B.==?. B.val_ False
                B.&&?. B.sqlBool_ (B.not_ (driverInfo.driverId `B.in_` (B.val_ . getId <$> driverIdsToBeExcluded)))
          )
          do
            person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
            driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
            pure (person, driverInfo)
  case result of
    Right x -> do
      let persons = fmap fst x
      p <- filter (\y -> y.merchantOperatingCityId == moCityId) <$> catMaybes <$> mapM fromTType' persons
      pure (personIdToDrivrId . (Person.id :: PersonE e -> Id Person) <$> p)
    Left _ -> pure []
  where
    personIdToDrivrId :: Id Person -> Id Driver

    personIdToDrivrId = cast

updateMerchantIdAndMakeAdmin :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id Merchant -> m ()
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.merchantId merchantId,
      Se.Set BeamP.role Person.ADMIN,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> m ()
updateName (Id personId) name = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName name,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonRec :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Person -> m ()
updatePersonRec (Id personId) person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.middleName $ person.middleName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.role $ person.role,
      Se.Set BeamP.gender $ person.gender,
      Se.Set BeamP.email $ person.email,
      Se.Set BeamP.hometown $ person.hometown,
      Se.Set BeamP.languagesSpoken $ person.languagesSpoken,
      Se.Set BeamP.identifier $ person.identifier,
      Se.Set BeamP.rating $ person.rating,
      Se.Set BeamP.language $ person.language,
      Se.Set BeamP.deviceToken $ person.deviceToken,
      Se.Set BeamP.merchantId $ getId person.merchantId,
      Se.Set BeamP.description $ person.description,
      Se.Set BeamP.updatedAt now,
      Se.Set BeamP.clientVersion (versionToText <$> person.clientVersion),
      Se.Set BeamP.bundleVersion (versionToText <$> person.bundleVersion)
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonVersions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      updateOneWithKV
        [ Se.Set BeamP.clientVersion mbClientVersionText,
          Se.Set BeamP.bundleVersion mbBundleVersionText,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]

updateDeviceToken :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe FCMRecipientToken -> m ()
updateDeviceToken (Id personId) mbDeviceToken = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.deviceToken mbDeviceToken,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateWhatsappNotificationEnrollStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateMobileNumberAndCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person -> m ()
updateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
      Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
      Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
      Se.Set BeamP.unencryptedMobileNumber $ person.unencryptedMobileNumber,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

setIsNewFalse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
setIsNewFalse (Id personId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.isNew False,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

deleteById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteById (Id personId) = deleteWithKV [Se.Is BeamP.id (Se.Eq personId)]

updateAverageRating :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Centesimal -> m ()
updateAverageRating (Id personId) newAverageRating = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.rating (Just newAverageRating),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateAlternateMobileNumberAndCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Person -> m ()
updateAlternateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.alternateMobileNumberEncrypted (person.alternateMobileNumber <&> unEncrypted . (.encrypted)),
      Se.Set BeamP.unencryptedAlternateMobileNumber person.unencryptedAlternateMobileNumber,
      Se.Set BeamP.alternateMobileNumberHash (person.alternateMobileNumber <&> (.hash)),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

findAllPersonWithDriverInfos :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DriverInformation] -> Id Merchant -> m [Person]
findAllPersonWithDriverInfos dInfos merchantId = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In (getId . DriverInfo.driverId <$> dInfos), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]

updateMediaId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe (Id MediaFile) -> m ()
updateMediaId (Id driverId) faceImageId = updateWithKV [Se.Set BeamP.faceImageId (getId <$> faceImageId)] [Se.Is BeamP.id $ Se.Eq driverId]

findAllMerchantIdByPhoneNo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DbHash -> m [Person]
findAllMerchantIdByPhoneNo countryCode mobileNumberHash =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
        ]
    ]

updateTotalEarnedCoins :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Int -> m ()
updateTotalEarnedCoins (Id driverId) coinValue = updateWithKV [Se.Set BeamP.totalEarnedCoins coinValue] [Se.Is BeamP.id $ Se.Eq driverId]

updateUsedCoins :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Int -> m ()
updateUsedCoins (Id driverId) usedCoinValue = updateWithKV [Se.Set BeamP.usedCoins usedCoinValue] [Se.Is BeamP.id $ Se.Eq driverId]
