{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.MerchantServiceUsageConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Call.Types (CallService)
import Kernel.External.Maps.Types
import Kernel.External.Notification.Types (NotificationService)
import Kernel.External.SMS (SmsService)
import Kernel.External.Ticket.Types (IssueTicketService)
import Kernel.External.Whatsapp.Types (WhatsappService)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Text,
    initiateCall :: B.C f CallService,
    getDistances :: B.C f MapsService,
    getRoutes :: B.C f MapsService,
    snapToRoad :: B.C f MapsService,
    getPlaceName :: B.C f MapsService,
    getPickupRoutes :: B.C f MapsService,
    getTripRoutes :: B.C f MapsService,
    getPlaceDetails :: B.C f MapsService,
    autoComplete :: B.C f MapsService,
    getDistancesForCancelRide :: B.C f MapsService,
    notifyPerson :: B.C f NotificationService,
    useFraudDetection :: B.C f Bool,
    smsProvidersPriorityList :: B.C f [SmsService],
    whatsappProvidersPriorityList :: B.C f [WhatsappService],
    issueTicketService :: B.C f IssueTicketService,
    enableDashboardSms :: B.C f Bool,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

merchantServiceUsageConfigTMod :: MerchantServiceUsageConfigT (B.FieldModification (B.TableField MerchantServiceUsageConfigT))
merchantServiceUsageConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      initiateCall = B.fieldNamed "initiate_call",
      getDistances = B.fieldNamed "get_distances",
      getRoutes = B.fieldNamed "get_routes",
      snapToRoad = B.fieldNamed "snap_to_road",
      getPlaceName = B.fieldNamed "get_place_name",
      getPickupRoutes = B.fieldNamed "get_pickup_routes",
      getTripRoutes = B.fieldNamed "get_trip_routes",
      getPlaceDetails = B.fieldNamed "get_place_details",
      autoComplete = B.fieldNamed "auto_complete",
      getDistancesForCancelRide = B.fieldNamed "get_distances_for_cancel_ride",
      notifyPerson = B.fieldNamed "notify_person",
      useFraudDetection = B.fieldNamed "use_fraud_detection",
      smsProvidersPriorityList = B.fieldNamed "sms_providers_priority_list",
      whatsappProvidersPriorityList = B.fieldNamed "whatsapp_providers_priority_list",
      issueTicketService = B.fieldNamed "issue_ticket_service",
      enableDashboardSms = B.fieldNamed "enable_dashboard_sms",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantId] [])

$(mkTableInstances ''MerchantServiceUsageConfigT "merchant_service_usage_config" "atlas_app")
