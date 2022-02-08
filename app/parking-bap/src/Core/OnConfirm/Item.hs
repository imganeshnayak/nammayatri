module Core.OnConfirm.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Price
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Item = Item
  { id :: Text,
    price :: Price,
    quantity :: Quantity
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
