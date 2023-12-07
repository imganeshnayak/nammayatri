{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search.Intent
  ( module Beckn.Types.Core.Taxi.Search.Intent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.Fulfillment
import Beckn.Types.Core.Taxi.Search.Payment
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data IntentV2 = IntentV2
  { fulfillment :: FulfillmentInfoV2,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema IntentV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

newtype Intent = Intent
  { fulfillment :: FulfillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Intent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
