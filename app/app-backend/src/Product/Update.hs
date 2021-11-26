module Product.Update where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration1.API.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Migration1.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Utils.Common

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContextMig1 req.context
    case req.contents of
      Left err -> logTagError "on_update req" $ "on_update error: " <> show err
      Right msg -> processOrder msg.order
    return Ack

processOrder :: DBFlow m r => OnUpdate.RideOrder -> m ()
processOrder (OnUpdate.TripAssigned taOrder) = do
  let rideBookingId = Id taOrder.id
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (rideBooking.status == SRB.CONFIRMED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- buildRide rideBooking
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.TRIP_ASSIGNED
    QRide.create ride
  where
    buildRide :: MonadFlow m => SRB.RideBooking -> m SRide.Ride
    buildRide rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let fulfillment = taOrder.fulfillment
          otp = fulfillment.otp
          driverName = fulfillment.agent.name
          driverMobileNumber = fulfillment.agent.phone
          driverRating = fulfillment.agent.rating
          driverRegisteredAt = fulfillment.agent.registered_at
      vehicleNumber <- fulfillment.vehicle.registration & fromMaybeM (InternalError "Vehicle registration number is not present.")
      vehicleColor <- fulfillment.vehicle.color & fromMaybeM (InternalError "Vehicle color is not present.")
      vehicleModel <- fulfillment.vehicle.model & fromMaybeM (InternalError "Vehicle model is not present.")
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            status = SRide.NEW,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            vehicleVariant = rideBooking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            ..
          }
processOrder (OnUpdate.RideStarted rsOrder) = do
  let rideBookingId = Id rsOrder.id
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- QRide.findByRBId rideBooking.id >>= fromMaybeM RideNotFound
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  DB.runSqlDBTransaction $ do
    QRide.updateStatus ride.id SRide.INPROGRESS
processOrder (OnUpdate.RideCompleted rcOrder) = do
  let rideBookingId = Id rcOrder.id
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- QRide.findByRBId rideBooking.id >>= fromMaybeM RideNotFound
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  let updRide =
        ride{status = SRide.COMPLETED,
             totalFare = Just $ realToFrac rcOrder.payment.params.amount,
             chargeableDistance = Just rcOrder.fulfillment.chargeable_distance
            }
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide