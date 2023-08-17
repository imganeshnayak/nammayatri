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

module Storage.Beam.Person where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Person as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash)
import Kernel.External.Maps (Language)
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.IdentifierType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.IdentifierType

instance FromBackendRow Postgres Domain.IdentifierType

instance IsString Domain.IdentifierType where
  fromString = show

instance FromField Domain.Gender where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Gender where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Gender

instance FromBackendRow Postgres Domain.Gender

instance IsString Domain.Gender where
  fromString = show

instance FromField Domain.Role where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Role where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Role

instance FromBackendRow Postgres Domain.Role

instance IsString Domain.Role where
  fromString = show

instance FromField OptApiMethods where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OptApiMethods where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be OptApiMethods

instance FromBackendRow Postgres OptApiMethods

deriving stock instance Ord OptApiMethods

instance IsString OptApiMethods where
  fromString = show

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f (Maybe Text),
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    role :: B.C f Domain.Role,
    gender :: B.C f Domain.Gender,
    identifierType :: B.C f Domain.IdentifierType,
    emailEncrypted :: B.C f (Maybe Text),
    emailHash :: B.C f (Maybe DbHash),
    unencryptedMobileNumber :: B.C f (Maybe Text),
    mobileNumberEncrypted :: B.C f (Maybe Text),
    mobileNumberHash :: B.C f (Maybe DbHash),
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    rating :: B.C f (Maybe Text),
    language :: B.C f (Maybe Language),
    isNew :: B.C f Bool,
    enabled :: B.C f Bool,
    blocked :: B.C f Bool,
    deviceToken :: B.C f (Maybe Text),
    notificationToken :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    whatsappNotificationEnrollStatus :: B.C f (Maybe OptApiMethods),
    createdAt :: B.C f Time.UTCTime,
    blockedAt :: B.C f (Maybe Time.LocalTime),
    blockedByRuleId :: B.C f (Maybe Text),
    updatedAt :: B.C f Time.UTCTime,
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text),
    hasTakenValidRide :: B.C f Bool,
    referralCode :: B.C f (Maybe Text),
    referredAt :: B.C f (Maybe Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Person = PersonT Identity

personTMod :: PersonT (B.FieldModification (B.TableField PersonT))
personTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      firstName = B.fieldNamed "first_name",
      middleName = B.fieldNamed "middle_name",
      lastName = B.fieldNamed "last_name",
      role = B.fieldNamed "role",
      gender = B.fieldNamed "gender",
      identifierType = B.fieldNamed "identifier_type",
      emailEncrypted = B.fieldNamed "email_encrypted",
      emailHash = B.fieldNamed "email_hash",
      unencryptedMobileNumber = B.fieldNamed "unencrypted_mobile_number",
      mobileNumberEncrypted = B.fieldNamed "mobile_number_encrypted",
      mobileNumberHash = B.fieldNamed "mobile_number_hash",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      passwordHash = B.fieldNamed "password_hash",
      identifier = B.fieldNamed "identifier",
      rating = B.fieldNamed "rating",
      language = B.fieldNamed "language",
      isNew = B.fieldNamed "is_new",
      enabled = B.fieldNamed "enabled",
      blocked = B.fieldNamed "blocked",
      deviceToken = B.fieldNamed "device_token",
      notificationToken = B.fieldNamed "notification_token",
      description = B.fieldNamed "description",
      merchantId = B.fieldNamed "merchant_id",
      whatsappNotificationEnrollStatus = B.fieldNamed "whatsapp_notification_enroll_status",
      createdAt = B.fieldNamed "created_at",
      blockedAt = B.fieldNamed "blocked_at",
      blockedByRuleId = B.fieldNamed "blocked_by_rule_id",
      updatedAt = B.fieldNamed "updated_at",
      bundleVersion = B.fieldNamed "bundle_version",
      clientVersion = B.fieldNamed "client_version",
      hasTakenValidRide = B.fieldNamed "has_taken_valid_ride",
      referralCode = B.fieldNamed "referral_code",
      referredAt = B.fieldNamed "referred_at"
    }

$(enableKVPG ''PersonT ['id] [['mobileNumberHash], ['emailHash], ['referralCode], ['deviceToken]])

$(mkTableInstances ''PersonT "person" "atlas_app")
