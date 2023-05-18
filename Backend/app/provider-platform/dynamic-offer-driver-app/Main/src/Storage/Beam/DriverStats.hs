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

module Storage.Beam.DriverStats where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Person (PersonTId)
import Storage.Beam.Instances ()

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

data DriverStatsT f = DriverStatsT
  { driverId :: B.C f Text,
    idleSince :: B.C f Time.UTCTime,
    totalRides :: B.C f Int,
    totalDistance :: B.C f Meters
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta DriverStatsT where
  modelFieldModification = driverStatsTMod
  modelTableName = "driver_stats"
  mkExprWithDefault _ = B.insertExpressions []

type DriverStats = DriverStatsT Identity

instance FromJSON DriverStats where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverStats where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverStats

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

driverStatsTMod :: DriverStatsT (B.FieldModification (B.TableField DriverStatsT))
driverStatsTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      idleSince = B.fieldNamed "idle_since",
      totalRides = B.fieldNamed "total_rides",
      totalDistance = B.fieldNamed "total_distance"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverStatsToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverStatsToHSModifiers =
  M.fromList
    []

driverStatsToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverStatsToPSModifiers =
  M.fromList
    []

defaultDriverStats :: DriverStats
defaultDriverStats =
  DriverStatsT
    { driverId = "",
      idleSince = defaultUTCDate,
      totalRides = 0,
      totalDistance = Meters 0
    }

instance Serialize DriverStats where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverStatsT ['driverId] [])
