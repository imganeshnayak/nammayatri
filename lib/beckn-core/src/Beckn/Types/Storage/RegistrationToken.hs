{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.RegistrationToken where

import Data.Aeson
import Data.Swagger
import qualified Data.Text as T
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)

data Medium
  = SMS
  | EMAIL
  deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Medium where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Medium where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RTEntityType
  = CUSTOMER
  | USER
  deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RTEntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RTEntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data LoginType
  = OTP
  | PASSWORD
  deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LoginType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres LoginType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RegistrationTokenT f = RegistrationToken
  { id :: B.C f Text,
    token :: B.C f Text,
    attempts :: B.C f Int,
    authMedium :: B.C f Medium,
    authType :: B.C f LoginType,
    authValueHash :: B.C f Text,
    verified :: B.C f Bool,
    authExpiry :: B.C f Int,
    tokenExpiry :: B.C f Int,
    entityId :: B.C f Text,
    entityType :: B.C f RTEntityType,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RegistrationToken = RegistrationTokenT Identity

type RegistrationTokenPrimaryKey = B.PrimaryKey RegistrationTokenT Identity

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f = RegistrationTokenPrimaryKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = RegistrationTokenPrimaryKey . id

deriving instance Show RegistrationToken

deriving instance Eq RegistrationToken

deriving instance ToJSON RegistrationToken

deriving instance FromJSON RegistrationToken

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RegistrationTokenT)
fieldEMod =
  B.setEntityName "registration_token"
    <> B.modifyTableFields
      B.tableModification
        { authMedium = "auth_medium",
          authType = "auth_type",
          authValueHash = "auth_value_hash",
          authExpiry = "auth_expiry",
          tokenExpiry = "token_expiry",
          entityId = "entity_id",
          entityType = "entity_type",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
