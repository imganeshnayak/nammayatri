{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.MerchantServiceUsageConfig where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Types.CommonImport (MapsService)
import Kernel.Types.Id
import Lib.SMS.Types
import Lib.Verification.Types (VerificationService)
import Lib.Whatsapp.Types

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Id Merchant,
    getDistances :: MapsService,
    getEstimatedPickupDistances :: MapsService,
    getRoutes :: MapsService,
    getPickupRoutes :: MapsService,
    getTripRoutes :: MapsService,
    snapToRoad :: MapsService,
    getPlaceName :: MapsService,
    getPlaceDetails :: MapsService,
    autoComplete :: MapsService,
    smsProvidersPriorityList :: [SmsService],
    whatsappProvidersPriorityList :: [WhatsappService],
    verificationService :: VerificationService,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
