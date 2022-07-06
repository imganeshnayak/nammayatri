module Product.RideBooking where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Domain.Types.AllocationEvent
import qualified Domain.Types.AllocationEvent as AllocationEvent
import Domain.Types.BookingLocation as DBLoc
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.RideRequest
import qualified Domain.Types.RideRequest as SRideRequest
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle as SVeh
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP (buildRideReq)
import qualified Product.Location as Location
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RentalFarePolicy as QRentalFP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Types.API.RideBooking as API
import Types.Error
import Utils.Common

rideBookingStatus :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId _ = withFlowHandlerAPI $ do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
  buildRideBookingStatusRes rideBooking

rideBookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList person mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  rbList <- QRB.findAllByOrg orgId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rbList

rideBookingCancel ::
  Id SRB.RideBooking ->
  SP.Person ->
  FlowHandler APISuccess
rideBookingCancel rideBookingId admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <-
    QOrg.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
  now <- getCurrentTime
  rideReq <- buildRideReq rideBookingId (org.shortId) SRideRequest.CANCELLATION now
  Esq.runTransaction $ RideRequest.create rideReq
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> rideBookingCancel : ") (show rideReq)
  return Success

getRideInfo :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler API.GetRideInfoRes
getRideInfo rideBookingId personId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideBookingId
  case mbNotification of
    Nothing -> return $ API.GetRideInfoRes Nothing
    Just notification -> do
      let notificationExpiryTime = notification.expiresAt
      rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
      driver <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let driverLatLong = Location.locationToLatLong driverLocation
      fromLocation <-
        QBLoc.findById rideBooking.fromLocationId
          >>= fromMaybeM LocationNotFound
      let fromLatLong = Location.locationToLatLong fromLocation
      toLocation <- case rideBooking.rideBookingDetails of
        SRB.OneWayDetails details -> QBLoc.findById details.toLocationId >>= fromMaybeM LocationNotFound . Just
        SRB.RentalDetails _ -> pure Nothing
      distanceDuration <- MapSearch.getDistance (Just MapSearch.CAR) driverLatLong fromLatLong
      return $
        API.GetRideInfoRes $
          Just $
            API.RideInfo
              { bookingId = rideBooking.id,
                pickupLoc = DBLoc.makeBookingLocationAPIEntity fromLocation,
                dropLoc = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
                etaForPickupLoc = secondsToMinutes $ distanceDuration.duration,
                distanceToPickupLoc = distanceDuration.distance,
                notificationExpiryTime = notificationExpiryTime,
                estimatedFare = rideBooking.estimatedFare,
                discount = rideBooking.discount,
                estimatedTotalFare = rideBooking.estimatedTotalFare
              }
  where
    driverId = cast personId

responseToEventType :: API.NotificationStatus -> AllocationEventType
responseToEventType API.ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType API.REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: Id SRB.RideBooking -> Id SP.Person -> API.SetDriverAcceptanceReq -> FlowHandler API.SetDriverAcceptanceRes
setDriverAcceptance rideBookingId personId req = withFlowHandlerAPI $ do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  rideBooking <-
    QRB.findById rideBookingId
      >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
  transporterOrg <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgDoesNotExist rideBooking.providerId.getId)
  guid <- generateGUID
  let driverResponse =
        API.DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideBookingId = rideBookingId,
            shortOrgId = transporterOrg.shortId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just $ encodeToText driverResponse
          }
  Esq.runTransaction $ do
    RideRequest.create rideRequest
    AllocationEvent.logAllocationEvent
      (responseToEventType response)
      rideBookingId
      (Just driverId)
  pure Success
  where
    response = req.response
    driverId = cast personId
    logMessage =
      "beckn:" <> rideBookingId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response

buildRideBookingStatusRes :: (EsqDBFlow m r, EncFlow m r) => SRB.RideBooking -> m API.RideBookingStatusRes
buildRideBookingStatusRes rideBooking = do
  fromLocation <- QBLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  let rbStatus = rideBooking.status
  now <- getCurrentTime
  rideAPIEntityList <- mapM (buildRideAPIEntity now) =<< QRide.findAllRideAPIEntityDataByRBId rideBooking.id
  fareBreakups <- QFareBreakup.findAllByRideBookingId rideBooking.id
  bookingDetails <- buildRideBookingAPIDetails rideBooking.rideBookingDetails

  return $
    API.RideBookingStatusRes
      { id = rideBooking.id,
        status = rbStatus,
        estimatedFare = rideBooking.estimatedFare,
        discount = rideBooking.discount,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        fromLocation = DBLoc.makeBookingLocationAPIEntity fromLocation,
        rideList = rideAPIEntityList,
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        bookingDetails,
        createdAt = rideBooking.createdAt,
        updatedAt = rideBooking.updatedAt
      }
  where
    buildRideAPIEntity :: (EsqDBFlow m r, EncFlow m r) => UTCTime -> (SRide.Ride, Maybe SVeh.Vehicle, Maybe SP.Person) -> m SRide.RideAPIEntity
    buildRideAPIEntity now (ride, mbVehicle, mbDriver) = do
      let vehicle = fromMaybe (vehicleDefault now) mbVehicle
      decDriver <- maybe (return $ driverDefault now) decrypt mbDriver
      return $ SRide.makeRideAPIEntity ride decDriver vehicle

    buildRideBookingAPIDetails :: EsqDBFlow m r => SRB.RideBookingDetails -> m API.RideBookingAPIDetails
    buildRideBookingAPIDetails = \case
      SRB.OneWayDetails SRB.OneWayRideBookingDetails {..} -> do
        toLocation' <- QBLoc.findById toLocationId >>= fromMaybeM LocationNotFound
        pure $
          API.OneWayAPIDetails
            API.OneWayRideBookingAPIDetails
              { toLocation = DBLoc.makeBookingLocationAPIEntity toLocation'
              }
      SRB.RentalDetails (SRB.RentalRideBookingDetails rentalFarePolicyId) -> do
        DRentalFP.RentalFarePolicy {..} <-
          QRentalFP.findById rentalFarePolicyId
            >>= fromMaybeM NoRentalFarePolicy
        pure $ API.RentalAPIDetails API.RentalRideBookingAPIDetails {..}

    driverDefault now =
      SP.Person
        { id = Id "[Driver deleted]",
          firstName = "[Driver deleted]",
          middleName = Nothing,
          lastName = Nothing,
          role = SP.DRIVER,
          gender = SP.FEMALE,
          identifierType = SP.EMAIL,
          email = Nothing,
          mobileNumber = Just "N/A",
          mobileCountryCode = Nothing,
          passwordHash = Nothing,
          identifier = Nothing,
          rating = Nothing,
          isNew = False,
          udf1 = Nothing,
          udf2 = Nothing,
          organizationId = Nothing,
          deviceToken = Nothing,
          description = Nothing,
          createdAt = now,
          updatedAt = now
        }
    vehicleDefault now =
      SV.Vehicle
        { id = Id "[Vehicle deleted]",
          organizationId = Id "N/A",
          variant = SV.SEDAN,
          model = "N/A",
          color = "N/A",
          registrationNo = "N/A",
          capacity = Nothing,
          category = Nothing,
          make = Nothing,
          size = Nothing,
          energyType = Nothing,
          registrationCategory = Nothing,
          createdAt = now,
          updatedAt = now
        }
