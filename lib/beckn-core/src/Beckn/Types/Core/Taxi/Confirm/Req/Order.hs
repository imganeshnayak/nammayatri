module Beckn.Types.Core.Taxi.Confirm.Req.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Req.Order,
  )
where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)

newtype Order = Order
  { items :: [OrderItem]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype OrderItem = OrderItem
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Order where
  example =
    Order
      { items = [OrderItem "quote_id"]
      }
