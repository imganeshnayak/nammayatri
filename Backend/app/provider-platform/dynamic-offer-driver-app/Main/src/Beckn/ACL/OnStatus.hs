{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus
  ( buildOnStatusMessage,
  )
where

import qualified Beckn.ACL.Common as Common
import Beckn.ACL.Common.Fulfillment (mkFulfillment)
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as BookingCancelledOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder as NewBookingOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as RideAssignedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as RideCompletedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as RideStartedOS
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FareParameters as DFParams
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import qualified SharedLogic.SyncRide as SyncRide
import Tools.Error

buildOnStatusMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  DStatus.OnStatusBuildReq ->
  m OnStatus.OnStatusMessage
buildOnStatusMessage (DStatus.NewBookingBuildReq {bookingId}) = do
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.NewBooking $
            NewBookingOS.NewBookingOrder
              { id = bookingId.getId,
                state = NewBookingOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideAssignedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) -- TODO reuse the same from on_update
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideAssigned $
            RideAssignedOS.RideAssignedOrder
              { id = booking.id.getId,
                state = RideAssignedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideStartedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup)
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideStarted $
            RideStartedOS.RideStartedOrder
              { id = booking.id.getId,
                state = RideStartedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideCompletedBuildReq {newRideInfo, rideCompletedInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let SyncRide.RideCompletedInfo {fareParams, paymentMethodInfo, paymentUrl} = rideCompletedInfo
  let arrivalTimeTagGroup = mkArrivalTimeTagGroup ride.driverArrivalTime
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let traveledDistance :: HighPrecMeters = ride.traveledDistance
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "ride_distance_details",
              name = "Ride Distance Details",
              list =
                [ Tags.Tag (Just False) (Just "chargeable_distance") (Just "Chargeable Distance") (Just $ show chargeableDistance),
                  Tags.Tag (Just False) (Just "traveled_distance") (Just "Traveled Distance") (Just $ show traveledDistance)
                ]
            }
        ]
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG (tagGroups <> arrivalTimeTagGroup))
  fare <- realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let currency = "INR"
      price =
        RideCompletedOS.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup =
        mkBreakupList (RideCompletedOS.BreakupItemPrice currency . fromIntegral) RideCompletedOS.BreakupItem fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType fareParams) -- TODO: Remove after roll out
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideCompleted
            RideCompletedOS.RideCompletedOrder
              { id = booking.id.getId,
                state = RideCompletedOS.orderState,
                quote =
                  RideCompletedOS.RideCompletedQuote
                    { price,
                      breakup
                    },
                payment =
                  Just
                    RideCompletedOS.Payment
                      { _type = maybe RideCompletedOS.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) paymentMethodInfo,
                        params =
                          RideCompletedOS.PaymentParams
                            { collected_by = maybe RideCompletedOS.BPP (Common.castDPaymentCollector . (.collectedBy)) paymentMethodInfo,
                              instrument = Nothing,
                              currency = "INR",
                              amount = Nothing
                            },
                        uri = paymentUrl
                      },
                fulfillment = fulfillment
              }
      }
buildOnStatusMessage DStatus.BookingCancelledBuildReq {bookingCancelledInfo, mbNewRideInfo} = do
  let SyncRide.BookingCancelledInfo {booking, cancellationSource} = bookingCancelledInfo
  fulfillment <- forM mbNewRideInfo $ \newRideInfo -> do
    let SyncRide.NewRideInfo {driver, image, vehicle, ride} = newRideInfo
    let arrivalTimeTagGroup = mkArrivalTimeTagGroup ride.driverArrivalTime
    mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup)
  pure
    OnStatus.OnStatusMessage
      { order =
          OnStatus.BookingCancelled $
            BookingCancelledOS.BookingCancelledOrder
              { id = booking.id.getId,
                state = BookingCancelledOS.orderState,
                cancellation_reason = castCancellationSource cancellationSource,
                fulfillment
              }
      }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOS.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOS.ByUser
  SBCR.ByDriver -> BookingCancelledOS.ByDriver
  SBCR.ByMerchant -> BookingCancelledOS.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOS.ByAllocator
  SBCR.ByApplication -> BookingCancelledOS.ByApplication

-- FIXME use for each order
-- let tagGroups =
--       [ Tags.TagGroup
--           { display = False,
--             code = "driver_arrived_info",
--             name = "Driver Arrived Info",
--             list = [Tags.Tag (Just False) (Just "arrival_time") (Just "Chargeable Distance") (show <$> arrivalTime) | isJust arrivalTime]
--           }
--       ]

-- buildDistanceTagGroup :: MonadFlow m => DRide.Ride -> m [Tags.TagGroup]
-- buildDistanceTagGroup ride = do
--   chargeableDistance :: HighPrecMeters <-
--     realToFrac <$> ride.chargeableDistance
--       & fromMaybeM (InternalError "Ride chargeable distance is not present.")
--   let traveledDistance :: HighPrecMeters = ride.traveledDistance
--   pure [ Tags.TagGroup
--           { display = False,
--             code = "ride_distance_details",
--             name = "Ride Distance Details",
--             list =
--               [ Tags.Tag (Just False) (Just "chargeable_distance") (Just "Chargeable Distance") (Just $ show chargeableDistance),
--                 Tags.Tag (Just False) (Just "traveled_distance") (Just "Traveled Distance") (Just $ show traveledDistance)
--               ]
--           }
--       ]

mkArrivalTimeTagGroup :: Maybe UTCTime -> [Tags.TagGroup]
mkArrivalTimeTagGroup arrivalTime =
  [ Tags.TagGroup
      { display = False,
        code = "driver_arrived_info",
        name = "Driver Arrived Info",
        list = [Tags.Tag (Just False) (Just "arrival_time") (Just "Chargeable Distance") (show <$> arrivalTime) | isJust arrivalTime]
      }
  ]
