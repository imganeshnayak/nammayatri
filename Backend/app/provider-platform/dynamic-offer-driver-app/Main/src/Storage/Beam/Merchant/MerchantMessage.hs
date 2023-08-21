{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.MerchantMessage where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Merchant.MerchantMessage as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.MessageKey where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.MessageKey where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MessageKey

instance FromBackendRow Postgres Domain.MessageKey

instance IsString Domain.MessageKey where
  fromString = show

data MerchantMessageT f = MerchantMessageT
  { merchantId :: B.C f Text,
    messageKey :: B.C f Domain.MessageKey,
    message :: B.C f Text,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantMessageT where
  data PrimaryKey MerchantMessageT f
    = Id (B.C f Domain.MessageKey)
    deriving (Generic, B.Beamable)
  primaryKey = Id . messageKey

instance ModelMeta MerchantMessageT where
  modelFieldModification = merchantMessageTMod
  modelTableName = "merchant_message"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MerchantMessage = MerchantMessageT Identity

instance FromJSON MerchantMessage where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MerchantMessage where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MerchantMessage

merchantMessageTMod :: MerchantMessageT (B.FieldModification (B.TableField MerchantMessageT))
merchantMessageTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      messageKey = B.fieldNamed "message_key",
      message = B.fieldNamed "message",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize MerchantMessage where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantMessageToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantMessageToHSModifiers =
  M.empty

merchantMessageToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantMessageToPSModifiers =
  M.empty

$(enableKVPG ''MerchantMessageT ['merchantId, 'messageKey] [])