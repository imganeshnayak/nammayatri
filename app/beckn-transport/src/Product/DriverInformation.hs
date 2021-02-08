{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import qualified App.Types as App
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App
import Beckn.Types.MapSearch
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.RegistrationToken (RegistrationToken, RegistrationTokenT (..))
import Beckn.Utils.Common (fromMaybeM500, getCurrTime, withFlowHandler)
import Data.List ((\\))
import Data.List.NonEmpty (unzip)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude hiding (unzip)
import qualified Models.ProductInstance as ModelPI
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrganization
import Storage.Queries.Person (findAllActiveDrivers, findAllByRoles)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QueryPI
import qualified Types.API.DriverInformation as DriverInformationAPI
import Types.App
import qualified Types.Storage.DriverStats as DriverStats

data ServiceHandle m = ServiceHandle
  { findActiveDrivers :: m [Person.Person],
    findRidesByStartTimeBuffer :: UTCTime -> NominalDiffTime -> [PI.ProductInstanceStatus] -> m [PI.ProductInstance],
    fetchDriversStats :: [DriverId] -> Integer -> m [DriverStats.DriverStats]
  }

getAvailableDriversInfo :: RegistrationToken -> UTCTime -> Integer -> App.FlowHandler DriverInformationAPI.ActiveDriversResponse
getAvailableDriversInfo _ time quantity = do
  let handle =
        ServiceHandle
          { findActiveDrivers = findAllActiveDrivers,
            findRidesByStartTimeBuffer = ModelPI.findByStartTimeBuffer Case.RIDEORDER,
            fetchDriversStats = QDriverStats.findByIdsInAscendingRidesOrder
          }
  withFlowHandler $ handleGetAvailableDriversInfo handle time quantity

handleGetAvailableDriversInfo :: (Monad m) => ServiceHandle m -> UTCTime -> Integer -> m DriverInformationAPI.ActiveDriversResponse
handleGetAvailableDriversInfo ServiceHandle {..} time quantity = do
  activeDriversIds <- fmap Person._id <$> findActiveDrivers
  freeDriversIds <- fetchFreeDriversIds activeDriversIds
  driversStats <- fetchDriversStats (map (DriverId . _getPersonId) freeDriversIds) quantity
  pure $ DriverInformationAPI.ActiveDriversResponse {active_drivers = map mapToResp driversStats}
  where
    fetchFreeDriversIds activeDriversIds = do
      ridesBuffer <- findRidesByStartTimeBuffer time 1 [PI.CONFIRMED, PI.INPROGRESS, PI.TRIP_ASSIGNED]
      let busyDriversIds = catMaybes $ PI._personId <$> ridesBuffer
      let freeDriversIds = filter (`notElem` busyDriversIds) activeDriversIds
      pure freeDriversIds
    mapToResp DriverStats.DriverStats {..} =
      DriverInformationAPI.DriverRidesInformation
        { driver_id = PersonId . _getDriverId $ _driverId,
          completed_rides_over_time = _completedRidesNumber,
          earnings_over_time = fromRational $ toRational _earnings
        }

updateDriversStats :: Text -> Integer -> App.FlowHandler APIResult.APIResult
updateDriversStats _auth quantityToUpdate = withFlowHandler $ do
  _ <- createNewDriversStats
  driversIdsWithOudatedStats <- fmap DriverStats._driverId <$> QDriverStats.fetchMostOutdatedDriversStats quantityToUpdate
  now <- getCurrTime
  let fromTime = addUTCTime (- timePeriod) now
  driversStats <- traverse (fetchDriverStatsById fromTime now) driversIdsWithOudatedStats
  traverse_ updateStats driversStats
  pure APIResult.Success
  where
    createNewDriversStats = do
      driversIds <- fmap (DriverId . _getPersonId . Person._id) <$> findAllByRoles [Person.DRIVER]
      driversStatsIds <- fmap DriverStats._driverId <$> QDriverStats.fetchAll
      let newDrivers = driversIds \\ driversStatsIds
      traverse_ QDriverStats.createInitialDriverStats newDrivers
    fetchDriverStatsById fromTime toTime driverId = do
      rides <- QueryPI.getDriverCompletedRides (PersonId $ driverId ^. #_getDriverId) fromTime toTime
      let earnings = foldr sumProductInstancesByPrice 0 rides
      pure (driverId, length rides, earnings)
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    updateStats (driverId, completedRides, earnings) = QDriverStats.update driverId completedRides earnings
    timePeriod = nominalDay -- Move into config if there will be a need

getInformation :: RegistrationToken -> App.FlowHandler DriverInformationAPI.DriverInformationResponse
getInformation RegistrationToken {..} = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = DriverId _EntityId
  person <- QPerson.findPersonById (PersonId _EntityId)
  personEntity <- Person.mkPersonRes person
  orgId <- person ^. #_organizationId & fromMaybeM500 "ORGANIZATION_ID_IS_NOT_PRESENT"
  organization <- QOrganization.findOrganizationById $ OrganizationId orgId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM500 "INVALID_DRIVER_ID"
  pure $
    DriverInformationAPI.DriverInformationResponse
      { transporter = organization,
        person = personEntity,
        driverInformation = driverInfo
      }

setActivity :: RegistrationToken -> Bool -> App.FlowHandler APIResult.APIResult
setActivity RegistrationToken {..} isActive = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = DriverId _EntityId
  QDriverInformation.updateActivity driverId isActive
  pure APIResult.Success

getRideInfo :: RegistrationToken -> Maybe ProductInstanceId -> App.FlowHandler DriverInformationAPI.GetRideInfoRes
getRideInfo RegistrationToken {..} mbProductInstanceId = withFlowHandler $ do
  let rideId = RideId . _getProductInstanceId <$> mbProductInstanceId
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideId
  case mbNotification of
    Nothing -> return emptyGetRideInfoRes
    Just notification -> do
      let productInstanceId = rideIdToProductInstanceId $ notification ^. #_rideId
          notificationTime = notification ^. #_notifiedAt
      driverNotificationExpiry <- getDriverNotificationExpiry
      productInstance <- QueryPI.findById productInstanceId
      driver <- QPerson.findPersonById personId
      driverLocation <- findLocationById $ driver ^. #_locationId
      fromLocation <- findLocationById $ productInstance ^. #_fromLocation
      toLocation <- findLocationById $ productInstance ^. #_toLocation
      let (fromLat, fromLong) = extractLatLong fromLocation
          (driverLat, driverLong) = extractLatLong driverLocation
      mbRoute <- getRoute driverLat driverLong fromLat fromLong
      return
        DriverInformationAPI.GetRideInfoRes
          { _productInstanceId = Just productInstanceId,
            _pickupLoc = fromLocation,
            _dropLoc = toLocation,
            _etaForPickupLoc = (`div` 60) . durationInS <$> mbRoute,
            _distanceToPickupLoc = distanceInM <$> mbRoute,
            _notificationExpiryTime = addUTCTime driverNotificationExpiry <$> notificationTime
          }
  where
    driverId = DriverId _EntityId
    personId = PersonId $ _getDriverId driverId
    rideIdToProductInstanceId rideId = ProductInstanceId $ rideId ^. #_getRideId
    getDriverNotificationExpiry = App.driverNotificationExpiry . App.driverAllocationConfig <$> ask
    findLocationById mbId = maybe (return Nothing) QLocation.findLocationById $ LocationId <$> mbId
    extractLatLong location = unzip (location >>= (\loc -> (,) <$> loc ^. #_lat <*> loc ^. #_long))
    getRoute fromLat fromLong toLat toLong = fromMaybe (return Nothing) $ Location.getRoute' <$> fromLat <*> fromLong <*> toLat <*> toLong
    emptyGetRideInfoRes =
      DriverInformationAPI.GetRideInfoRes
        { _productInstanceId = mbProductInstanceId,
          _pickupLoc = Nothing,
          _dropLoc = Nothing,
          _etaForPickupLoc = Nothing,
          _distanceToPickupLoc = Nothing,
          _notificationExpiryTime = Nothing
        }
