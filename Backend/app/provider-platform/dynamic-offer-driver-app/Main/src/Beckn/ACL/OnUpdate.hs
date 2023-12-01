{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateMessage,
    OnUpdateBuildReq (..),
  )
where

import qualified Beckn.ACL.Common as Common
import Beckn.ACL.Common.Fulfillment (mkFulfillment)
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent as DriverArrivedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent as EstimateRepetitionOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.NewMessageEvent as NewMessageOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import Tools.Error

data OnUpdateBuildReq
  = RideAssignedBuildReq
      { driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        ride :: DRide.Ride,
        booking :: DRB.Booking,
        image :: Maybe Text
      }
  | RideStartedBuildReq
      { driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        ride :: DRide.Ride,
        booking :: DRB.Booking
      }
  | RideCompletedBuildReq
      { ride :: DRide.Ride,
        driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        booking :: DRB.Booking,
        fareParams :: Fare.FareParameters,
        paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
        paymentUrl :: Maybe Text
      }
  | BookingCancelledBuildReq
      { booking :: DRB.Booking,
        cancellationSource :: SBCR.CancellationSource
      }
  | DriverArrivedBuildReq
      { ride :: DRide.Ride,
        driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        booking :: DRB.Booking,
        arrivalTime :: Maybe UTCTime
      }
  | EstimateRepetitionBuildReq
      { ride :: DRide.Ride,
        booking :: DRB.Booking,
        estimateId :: Id DEst.Estimate,
        cancellationSource :: SBCR.CancellationSource
      }
  | NewMessageBuildReq
      { ride :: DRide.Ride,
        driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        booking :: DRB.Booking,
        message :: Text
      }

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage RideAssignedBuildReq {..} = do
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) image Nothing
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideAssigned $
            RideAssignedOU.RideAssignedEvent
              { id = booking.id.getId,
                state = "ACTIVE",
                ..
              },
        update_target = "order.fufillment.state.code, order.fulfillment.agent, order.fulfillment.vehicle" <> ", order.fulfillment.start.authorization" -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
      }
buildOnUpdateMessage RideStartedBuildReq {..} = do
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) Nothing Nothing
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideStarted $
            RideStartedOU.RideStartedEvent
              { id = booking.id.getId,
                ..
              },
        update_target = "order.fufillment.state.code"
      }
buildOnUpdateMessage req@RideCompletedBuildReq {} = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> req.ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let traveledDistance :: HighPrecMeters = req.ride.traveledDistance
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
  fulfillment <- mkFulfillment (Just req.driver) req.ride req.booking (Just req.vehicle) Nothing (Just $ Tags.TG tagGroups)
  fare <- realToFrac <$> req.ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let currency = "INR"
      price =
        RideCompletedOU.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup =
        mkBreakupList (OnUpdate.BreakupItemPrice currency . fromIntegral) OnUpdate.BreakupItem req.fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType req.fareParams) -- TODO: Remove after roll out
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideCompleted
            RideCompletedOU.RideCompletedEvent
              { id = req.booking.id.getId,
                quote =
                  RideCompletedOU.RideCompletedQuote
                    { price,
                      breakup
                    },
                payment =
                  Just
                    OnUpdate.Payment
                      { _type = maybe OnUpdate.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) req.paymentMethodInfo,
                        params =
                          OnUpdate.PaymentParams
                            { collected_by = maybe OnUpdate.BPP (Common.castDPaymentCollector . (.collectedBy)) req.paymentMethodInfo,
                              instrument = Nothing,
                              currency = "INR",
                              amount = Nothing
                            },
                        uri = req.paymentUrl
                      },
                fulfillment = fulfillment
              },
        update_target = "order.payment, order.quote, order.fulfillment.tags, order.fulfillment.state.tags"
      }
buildOnUpdateMessage BookingCancelledBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.BookingCancelled $
            BookingCancelledOU.BookingCancelledEvent
              { id = booking.id.getId,
                state = "CANCELLED",
                cancellation_reason = castCancellationSource cancellationSource
              },
        update_target = "state,fufillment.state.code"
      }
buildOnUpdateMessage DriverArrivedBuildReq {..} = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "driver_arrived_info",
              name = "Driver Arrived Info",
              list = [Tags.Tag (Just False) (Just "arrival_time") (Just "Chargeable Distance") (show <$> arrivalTime) | isJust arrivalTime]
            }
        ]
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups)
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.DriverArrived $
            DriverArrivedOU.DriverArrivedEvent
              { id = ride.bookingId.getId,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }
buildOnUpdateMessage EstimateRepetitionBuildReq {..} = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "previous_cancellation_reasons",
              name = "Previous Cancellation Reasons",
              list = [Tags.Tag (Just False) (Just "cancellation_reason") (Just "Chargeable Distance") (Just . show $ castCancellationSource cancellationSource)]
            }
        ]
  fulfillment <- mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups)
  let item = EstimateRepetitionOU.Item {id = estimateId.getId}
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.EstimateRepetition $
            EstimateRepetitionOU.EstimateRepetitionEvent
              { id = booking.id.getId,
                item = item,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.tags"
      }
buildOnUpdateMessage NewMessageBuildReq {..} = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "driver_new_message",
              name = "Driver New Message",
              list = [Tags.Tag (Just False) (Just "message") (Just "New Message") (Just message)]
            }
        ]
  fulfillment <- mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups)
  return $
    OnUpdate.OnUpdateMessage
      { update_target = "order.fufillment.state.code, order.fulfillment.tags",
        order =
          OnUpdate.NewMessage $
            NewMessageOU.NewMessageEvent
              { id = ride.bookingId.getId,
                fulfillment = fulfillment
              }
      }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByMerchant -> BookingCancelledOU.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication
