{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Common (generateGUID)
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Error as Err
import Beckn.Types.Core.Item
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Operator
import Beckn.Types.Core.Payment
import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.Person
import Beckn.Types.Core.Price
import Beckn.Types.Core.Quotation
import Beckn.Types.Core.Tracking
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Status
import Beckn.Types.FMD.API.Track
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (throwJsonError400)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Control.Lens ((?~))
import qualified Data.Text as T
import EulerHS.Prelude hiding (drop)
import External.Dunzo.Types
import Types.Wrapper
import Utils.Common (getClientConfig)

getDunzoConfig :: Organization -> Flow DunzoConfig
getDunzoConfig org = do
  config <- getClientConfig org
  case config of
    Dunzo dzConfig -> return dzConfig

mkQuoteReqFromSearch :: SearchReq -> Flow QuoteReq
mkQuoteReqFromSearch SearchReq {..} = do
  let intent = message ^. #intent
      pickups = intent ^. #_pickups
      drops = intent ^. #_drops
  case (pickups, drops) of
    ([pickup], [drop]) ->
      case (pickup ^. #_location . #_gps, drop ^. #_location . #_gps) of
        (Just pgps, Just dgps) -> do
          plat <- readCoord (pgps ^. #lat)
          plon <- readCoord (pgps ^. #lon)
          dlat <- readCoord (dgps ^. #lat)
          dlon <- readCoord (dgps ^. #lon)
          return $
            QuoteReq
              { pickup_lat = plat,
                pickup_lng = plon,
                drop_lat = dlat,
                drop_lng = dlon,
                category_id = "pickup_drop"
              }
        (Just _, Nothing) -> dropLocationNotFound
        _ -> pickupLocationNotFound
    ([_], _) -> oneDropLocationExpected
    _ -> onePickupLocationExpected
  where
    onePickupLocationExpected = throwJsonError400 "ERR" "ONE_PICKUP_LOCATION_EXPECTED"
    oneDropLocationExpected = throwJsonError400 "ERR" "ONE_DROP_LOCATION_EXPECTED"
    pickupLocationNotFound = throwJsonError400 "ERR" "PICKUP_LOCATION_NOT_FOUND"
    dropLocationNotFound = throwJsonError400 "ERR" "DROP_LOCATION_NOT_FOUND"

mkQuoteReqFromSelect :: SelectReq -> Flow QuoteReq
mkQuoteReqFromSelect SelectReq {..} = do
  let tasks = message ^. (#order . #_tasks)
      task = head tasks
      pickup = task ^. (#_pickup . #_location)
      drop = task ^. (#_drop . #_location)
      pgps = pickup ^. #_gps
      dgps = drop ^. #_gps
  plat <- readCoord (fromJust pgps ^. #lat)
  plon <- readCoord (fromJust pgps ^. #lon)
  dlat <- readCoord (fromJust dgps ^. #lat)
  dlon <- readCoord (fromJust dgps ^. #lon)
  return $
    QuoteReq
      { pickup_lat = plat,
        pickup_lng = plon,
        drop_lat = dlat,
        drop_lng = dlon,
        category_id = "pickup_drop"
      }

readCoord :: Text -> Flow Double
readCoord text = do
  let mCoord = readMaybe $ T.unpack text
  maybe (throwJsonError400 "ERR" "LOCATION_READ_ERROR") pure mCoord

mkOnSearchErrReq :: Organization -> Context -> Error -> Flow OnSearchReq
mkOnSearchErrReq _ context Error {..} = do
  now <- getCurrentTimeUTC
  cid <- generateGUID
  return $
    OnSearchReq
      { context = context,
        message = OnSearchServices (catalog cid now),
        error = Just err
      }
  where
    catalog cid now =
      Catalog
        { _id = cid,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [],
          _offers = []
        }

    err =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnSearchReq :: Organization -> Context -> QuoteRes -> Flow OnSearchReq
mkOnSearchReq _ context res@QuoteRes {..} = do
  now <- getCurrentTimeUTC
  cid <- generateGUID
  itemid <- generateGUID
  return $
    OnSearchReq
      { context = context,
        message = OnSearchServices (catalog cid itemid now),
        error = Nothing
      }
  where
    catalog cid itemid now =
      Catalog
        { _id = cid,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [mkSearchItem itemid res],
          _offers = []
        }

updateContext :: Context -> Text -> Text -> Context
updateContext Context {..} bpId bpNwAddress = Context {_bpp_nw_address = Just bpNwAddress, _bpp_id = Just bpId, ..}

mkSearchItem :: Text -> QuoteRes -> Item
mkSearchItem itemId QuoteRes {..} =
  Item
    { _id = itemId,
      _parent_item_id = Nothing,
      _descriptor = descriptor,
      _price = price,
      _model_id = Nothing,
      _category_id = Just category_id,
      _brand_id = Nothing,
      _promotional = False,
      _ttl = Just $ eta ^. #pickup + eta ^. #dropoff, -- FIX this
      _tags = []
    }
  where
    price =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just value,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }
    value = convertAmountToDecimalValue (Amount $ toRational estimated_price)
    descriptor = Descriptor n n n n n [] n n
    n = Nothing

mkOnSelectReq :: SelectReq -> QuoteRes -> Flow OnSelectReq
mkOnSelectReq req@SelectReq {..} QuoteRes {..} = do
  qid <- generateGUID
  let order = req ^. (#message . #order)
      price = mkPrice estimated_price
      quotation = Quotation {_id = qid, _price = price, _ttl = Nothing}
  return $
    OnSelectReq
      { context = context,
        message = OnSelectMessage order (Just quotation),
        error = Nothing
      }
  where
    mkPrice estimatedPrice =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }

mkOnSelectErrReq :: SelectReq -> Error -> Flow OnSelectReq
mkOnSelectErrReq req Error {..} = do
  let context = req ^. #context
  let order = req ^. (#message . #order)
  return $
    OnSelectReq
      { context = context,
        message = OnSelectMessage order Nothing,
        error = Just mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnInitReq :: Text -> Order -> PaymentPolicy -> InitReq -> QuoteRes -> Flow OnInitReq
mkOnInitReq orderId order terms req QuoteRes {..} = do
  let billing = req ^. (#message . #order . #_billing)
  return $
    OnInitReq
      { context = req ^. #context,
        message = InitResMessage (order & #_id ?~ orderId & #_payment ?~ mkPayment & #_billing .~ billing),
        error = Nothing
      }
  where
    mkPayment =
      Payment
        { _transaction_id = Nothing,
          _type = Just "PRE-FULFILLMENT",
          _payer = Nothing,
          _payee = Nothing,
          _method = "RTGS",
          _amount = price,
          _state = Nothing,
          _due_date = Nothing,
          _duration = Nothing,
          _terms = Just terms
        }

    price =
      MonetaryValue
        { _currency = "INR",
          _value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }

mkOnInitErrReq :: InitReq -> Error -> Flow OnInitReq
mkOnInitErrReq req Error {..} = do
  let order = req ^. (#message . #order)
  return $
    OnInitReq
      { context = req ^. #context,
        message = InitResMessage order,
        error = Just mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnStatusReq :: StatusReq -> Text -> Order -> TaskStatus -> Flow OnStatusReq
mkOnStatusReq req orgName order status = do
  now <- getCurrentTimeUTC
  return $
    OnStatusReq
      { context = req ^. #context,
        message = StatusResMessage (updateOrder now),
        error = Nothing
      }
  where
    updateOrder cTime =
      order & #_state ?~ show (status ^. #state)
        & #_updated_at .~ cTime
        & #_tasks .~ (updateTask cTime <$> (order ^. #_tasks))

    updateTask cTime task =
      task & #_agent .~ (getAgent <$> status ^. #runner)
        & #_state .~ show (status ^. #state)
        & #_updated_at ?~ cTime

    getAgent runner =
      Operator
        { _name = Name n n (runner ^. #name) n n n,
          _image = n,
          _dob = n,
          _organization_name = Just orgName,
          _gender = n,
          _email = n,
          _phones = [runner ^. #phone_number],
          _experience = n
        }

    n = Nothing

mkOnStatusErrReq :: StatusReq -> Order -> Error -> Flow OnStatusReq
mkOnStatusErrReq req order Error {..} =
  return $
    OnStatusReq
      { context = req ^. #context,
        message = StatusResMessage order,
        error = Just mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnTrack :: TrackReq -> Flow OnTrackReq
mkOnTrack req = do
  return $
    OnTrackReq
      { context = req ^. #context,
        message = TrackResMessage tracking,
        error = Just mkError
      }
  where
    tracking =
      Tracking
        { _url = Nothing,
          _required_params = Nothing,
          _metadata = Nothing
        }
    -- TODO: fix this after dunzo sends tracking url in api
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = "FMD000",
          _path = Nothing,
          _message = Just "NO_TRACKING_URL"
        }
