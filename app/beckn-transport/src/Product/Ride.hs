module Product.Ride where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.BookingLocation as DBLoc
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler API.DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  API.DriverRideListRes <$> traverse buildDriverRideRes rideData

buildDriverRideRes :: (SRide.Ride, SRB.RideBooking) -> Flow API.DriverRideRes
buildDriverRideRes (ride, rideBooking) = do
  fromLocation <- QBLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- case rideBooking.rideBookingDetails of
    SRB.OneWayDetails details -> QBLoc.findById details.toLocationId >>= fromMaybeM LocationNotFound . Just
    SRB.RentalDetails _ -> pure Nothing

  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverNumber <- SP.getPersonNumber driver
  pure
    API.DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DBLoc.makeBookingLocationAPIEntity fromLocation,
        toLocation = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
        estimatedFare = rideBooking.estimatedFare,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        discount = rideBooking.discount,
        driverName = driver.firstName,
        driverNumber = driverNumber,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = vehicle.color,
        vehicleVariant = vehicle.variant,
        vehicleModel = vehicle.model,
        computedFare = ride.fare,
        computedTotalFare = ride.totalFare,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
