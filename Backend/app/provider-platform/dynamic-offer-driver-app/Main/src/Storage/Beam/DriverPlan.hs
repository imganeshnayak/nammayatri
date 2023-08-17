{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DriverPlan where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import qualified Domain.Types.DriverPlan as Domain
-- import Domain.Types.Person (Person)
import qualified Domain.Types.Plan as DPlan
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common
import Lib.Utils ()
import Sequelize

data DriverPlanT f = DriverPlanT
  { driverId :: B.C f Text,
    planId :: B.C f Text,
    planType :: B.C f DPlan.PaymentMode,
    mandateId :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance FromBackendRow Postgres DPlan.PaymentMode

instance FromField DPlan.PaymentMode where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DPlan.PaymentMode where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DPlan.PaymentMode

instance B.Table DriverPlanT where
  data PrimaryKey DriverPlanT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DriverPlan = DriverPlanT Identity

driverPlanTMod :: DriverPlanT (B.FieldModification (B.TableField DriverPlanT))
driverPlanTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      planId = B.fieldNamed "plan_id",
      planType = B.fieldNamed "plan_type",
      mandateId = B.fieldNamed "mandate_id",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverPlanT ['driverId] [])
$(mkTableInstances ''DriverPlanT "driver_plan" "atlas_driver_offer_bpp")
