{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Vehicle where

import Kernel.Prelude

newtype Vehicle = Vehicle
  { category :: VehicleVariant
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )

-- FIXME
-- import Data.Aeson as A
-- import Data.OpenApi hiding (Example, example, name, tags)
-- import Kernel.Prelude
-- import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- data Vehicle = Vehicle
--   { model :: Text,
--     variant :: Text,
--     color :: Text,
--     registration :: Text
--   }
--   deriving (Generic, FromJSON, ToJSON, Show)

-- instance ToSchema Vehicle where
--   declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
