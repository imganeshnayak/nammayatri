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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Payment.PaymentTransaction where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data PaymentTransactionT f = PaymentTransactionT
  { id :: B.C f Text,
    txnUUID :: B.C f Text,
    paymentMethodType :: B.C f Text,
    paymentMethod :: B.C f Text,
    respMessage :: B.C f (Maybe Text),
    respCode :: B.C f (Maybe Text),
    gatewayReferenceId :: B.C f (Maybe Text),
    orderId :: B.C f Text,
    merchantId :: B.C f Text,
    amount :: B.C f HighPrecMoney,
    currency :: B.C f Payment.Currency,
    dateCreated :: B.C f (Maybe Time.UTCTime),
    statusId :: B.C f Int,
    status :: B.C f Payment.TransactionStatus,
    juspayResponse :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentTransactionT where
  data PrimaryKey PaymentTransactionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PaymentTransaction = PaymentTransactionT Identity

paymentTransactionTMod :: PaymentTransactionT (B.FieldModification (B.TableField PaymentTransactionT))
paymentTransactionTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      txnUUID = B.fieldNamed "txn_uuid",
      paymentMethodType = B.fieldNamed "payment_method_type",
      paymentMethod = B.fieldNamed "payment_method",
      respMessage = B.fieldNamed "resp_message",
      respCode = B.fieldNamed "resp_code",
      gatewayReferenceId = B.fieldNamed "gateway_reference_id",
      orderId = B.fieldNamed "order_id",
      merchantId = B.fieldNamed "merchant_id",
      amount = B.fieldNamed "amount",
      currency = B.fieldNamed "currency",
      dateCreated = B.fieldNamed "date_created",
      statusId = B.fieldNamed "status_id",
      status = B.fieldNamed "status",
      juspayResponse = B.fieldNamed "juspay_response",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''PaymentTransactionT ['id] [])

$(mkTableInstances ''PaymentTransactionT "payment_transaction" "atlas_app")