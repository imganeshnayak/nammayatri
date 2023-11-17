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
import qualified Beckn.Types.Core.Taxi.Common.FulfillmentInfo as RideFulfillment
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as BookingCancelledOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder as NewBookingOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as RideAssignedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as RideCompletedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as RideStartedOS
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import qualified SharedLogic.SyncRide as SyncRide
import Tools.Error

mkFullfillment ::
  (EsqDBFlow m r, EncFlow m r) =>
  Maybe SP.Person ->
  DRide.Ride ->
  DRB.Booking ->
  Maybe SVeh.Vehicle ->
  Maybe Text ->
  Maybe Tags.TagGroups ->
  m RideFulfillment.FulfillmentInfo
mkFullfillment mbDriver ride booking mbVehicle mbImage tags = do
  agent <-
    forM mbDriver $ \driver -> do
      let agentTags =
            [ Tags.TagGroup
                { display = False,
                  code = "driver_details",
                  name = "Driver Details",
                  list =
                    [ Tags.Tag (Just False) (Just "registered_at") (Just "Registered At") (Just $ show driver.createdAt),
                      Tags.Tag (Just False) (Just "rating") (Just "rating") (show <$> driver.rating)
                    ]
                }
            ]
      mobileNumber <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
      name <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      pure $
        RideAssignedOS.Agent
          { name = name,
            rateable = True,
            phone = Just mobileNumber,
            image = mbImage,
            tags = Just $ Tags.TG agentTags
          }
  let veh =
        mbVehicle <&> \vehicle ->
          RideAssignedOS.Vehicle
            { model = vehicle.model,
              variant = show vehicle.variant,
              color = vehicle.color,
              registration = vehicle.registrationNo
            }
  let authorization =
        RideAssignedOS.Authorization
          { _type = "OTP",
            token = ride.otp
          }
  pure $
    RideAssignedOS.FulfillmentInfo
      { id = ride.id.getId,
        start =
          RideAssignedOS.StartInfo
            { authorization =
                case booking.bookingType of
                  DRB.SpecialZoneBooking -> Just authorization
                  DRB.NormalBooking -> Just authorization, -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
              location =
                RideAssignedOS.Location
                  { gps = RideAssignedOS.Gps {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                  },
              time = Nothing
            },
        end =
          RideAssignedOS.EndInfo
            { location =
                RideAssignedOS.Location
                  { gps = RideAssignedOS.Gps {lat = booking.toLocation.lat, lon = booking.toLocation.lon} -- assuming locations will always be in correct order in list
                  },
              time = Nothing
            },
        agent,
        _type = if booking.bookingType == DRB.NormalBooking then RideAssignedOS.RIDE else RideAssignedOS.RIDE_OTP,
        vehicle = veh,
        ..
      }

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
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) -- TODO reuse the same from on_update
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
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup)
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
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG (tagGroups <> arrivalTimeTagGroup))
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
    mkFullfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup)
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
