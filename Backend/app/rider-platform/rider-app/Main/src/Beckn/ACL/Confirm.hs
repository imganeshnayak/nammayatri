{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Confirm (buildConfirmReq, buildConfirmBusReq) where

import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Control.Lens ((%~))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

buildConfirmReq ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildContext Context.MOBILITY Context.CONFIRM messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  message <- mkConfirmMessage res
  pure $ BecknReq context message

buildConfirmBusReq ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmBusReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildContext Context.PUBLIC_TRANSPORT Context.CONFIRM messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  message <- mkConfirmBusMessage res
  pure $ BecknReq context message

mkConfirmMessage :: (MonadFlow m) => DOnInit.OnInitRes -> m Confirm.ConfirmMessage
mkConfirmMessage res = do
  let vehicleVariant = castVehicleVariant res.vehicleVariant
  pure
    Confirm.ConfirmMessage
      { order =
          Confirm.Order
            { id = getId $ fromJust res.bppBookingId,
              items =
                [ Confirm.OrderItem
                    { id = res.itemId,
                      price = Nothing
                    }
                ],
              fulfillment = mkFulfillment res.fulfillmentId fulfillmentType res.fromLocation res.mbToLocation res.riderPhoneCountryCode res.riderPhoneNumber res.mbRiderName vehicleVariant,
              payment = mkPayment res.estimatedTotalFare res.paymentUrl,
              quote =
                Confirm.Quote
                  { price =
                      Confirm.QuotePrice
                        { value = fromIntegral res.estimatedFare,
                          offered_value = Just $ fromIntegral res.estimatedTotalFare,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
              provider =
                res.driverId >>= \dId ->
                  Just
                    Confirm.Provider
                      { id = dId,
                        descriptor = Nothing
                      },
              billing =
                Just
                  Confirm.Billing
                    { name = Nothing,
                      email = Nothing,
                      phone = Nothing
                    }
            }
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> Confirm.SEDAN
      VehVar.SUV -> Confirm.SUV
      VehVar.HATCHBACK -> Confirm.HATCHBACK
      VehVar.AUTO_RICKSHAW -> Confirm.AUTO_RICKSHAW
      VehVar.TAXI -> Confirm.TAXI
      VehVar.TAXI_PLUS -> Confirm.TAXI_PLUS
      VehVar.BUS -> Confirm.BUS
    fulfillmentType = case res.bookingDetails of
      Just bookingDetails ->
        case bookingDetails of
          DRB.OneWaySpecialZoneDetails _ -> Confirm.RIDE_OTP
          _ -> Confirm.RIDE
      Nothing -> Confirm.RIDE

mkConfirmBusMessage :: (MonadFlow m) => DOnInit.OnInitRes -> m Confirm.ConfirmMessage
mkConfirmBusMessage res =
  pure
    Confirm.ConfirmMessage
      { order =
          Confirm.Order
            { id = getId $ fromJust res.ticketId,
              items =
                [ Confirm.OrderItem
                    { id = res.itemId,
                      price = Nothing
                    }
                ],
              fulfillment = mkFulfillment res.fulfillmentId Confirm.RIDE res.fromLocation res.mbToLocation Nothing Nothing Nothing Confirm.BUS,
              payment = mkPayment res.estimatedTotalFare Nothing,
              quote =
                Confirm.Quote
                  { price =
                      Confirm.QuotePrice
                        { value = fromIntegral res.estimatedFare,
                          offered_value = Nothing,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
              provider =
                Just
                  Confirm.Provider
                    { id = res.bppId,
                      descriptor = Nothing
                    },
              billing =
                Just
                  Confirm.Billing
                    { name = Nothing, -- TODO : Add billing to ticket
                      email = Nothing,
                      phone = Nothing
                    }
            }
      }

mkFulfillment :: Maybe Text -> Confirm.FulfillmentType -> DL.Location -> Maybe DL.Location -> Maybe Text -> Maybe Text -> Maybe Text -> Confirm.VehicleVariant -> Confirm.FulfillmentInfo
mkFulfillment fulfillmentId fulfillmentType startLoc mbStopLoc riderPhoneCountryCode riderPhoneNumber mbRiderName vehicleVariant =
  Confirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      start =
        Confirm.StartInfo
          { location =
              Confirm.Location
                { gps =
                    Confirm.Gps
                      { lat = startLoc.lat,
                        lon = startLoc.lon
                      },
                  address = Just $ mkAddress startLoc.address,
                  descriptor = Nothing
                },
            authorization = Nothing
          },
      end =
        mbStopLoc <&> \stopLoc ->
          Confirm.StopInfo
            { location =
                Confirm.Location
                  { gps =
                      Confirm.Gps
                        { lat = stopLoc.lat,
                          lon = stopLoc.lon
                        },
                    address = Just $ mkAddress stopLoc.address,
                    descriptor = Nothing
                  }
            },
      customer =
        Confirm.Customer
          { contact =
              Confirm.Contact
                { phone =
                    Confirm.Phone
                      { phoneNumber = fromJust riderPhoneNumber,
                        phoneCountryCode = fromJust riderPhoneCountryCode
                      }
                },
            person =
              mbRiderName <&> \riderName ->
                Confirm.OrderPerson
                  { name = riderName
                  }
          },
      vehicle =
        Confirm.Vehicle
          { category = vehicleVariant
          }
    }

mkAddress :: DLA.LocationAddress -> Confirm.Address
mkAddress DLA.LocationAddress {..} =
  Confirm.Address
    { area_code = areaCode,
      locality = area,
      ward = ward,
      door = door,
      ..
    }

mkPayment :: Money -> Maybe Text -> Confirm.Payment
mkPayment estimatedTotalFare uri =
  Confirm.Payment
    { _type = Confirm.ON_FULFILLMENT,
      params =
        Just
          Confirm.PaymentParams
            { collected_by = Confirm.BPP,
              instrument = Nothing,
              currency = "INR",
              amount = Just $ realToFrac estimatedTotalFare,
              transaction_id = Nothing
            },
      uri,
      tl_method = Nothing,
      status = Nothing
    }
