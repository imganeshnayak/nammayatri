{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.RideCompletedPayment
  ( module Beckn.Types.Core.Taxi.Common.RideCompletedPayment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentCollector as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentInstrument as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Kernel.Prelude
import Kernel.Utils.JSON as JSON
import Kernel.Utils.Schema

data RideCompletedPayment = RideCompletedPayment
  { collected_by :: Maybe PaymentCollector,
    _type :: Maybe PaymentType,
    instrument :: Maybe PaymentInstrument, -- FIXME find proper fields
    time :: TimeDuration, -- FIXME: what is this?
    uri :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON RideCompletedPayment where
  parseJSON = genericParseJSON JSON.stripPrefixUnderscoreIfAny

instance ToJSON RideCompletedPayment where
  toJSON = genericToJSON JSON.stripPrefixUnderscoreIfAny

instance ToSchema RideCompletedPayment where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions JSON.stripPrefixUnderscoreIfAny
