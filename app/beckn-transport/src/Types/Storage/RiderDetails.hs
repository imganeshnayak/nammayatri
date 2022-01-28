{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RiderDetails where

import Beckn.External.Encryption
import Beckn.Types.Id
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data RiderDetailsTE e f = RiderDetails
  { id :: B.C f (Id RiderDetails),
    mobileCountryCode :: B.C f Text,
    mobileNumber :: EncryptedHashedField e f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic)

type RiderDetailsT = RiderDetailsTE 'AsEncrypted

type RiderDetails = RiderDetailsT Identity

type RiderDetailsDecrypted = RiderDetailsTE 'AsUnencrypted Identity

type RiderDetailsPrimaryKey = B.PrimaryKey RiderDetailsT Identity

instance B.Beamable RiderDetailsT

instance B.Table RiderDetailsT where
  data PrimaryKey RiderDetailsT f = RiderDetailsPrimaryKey (B.C f (Id RiderDetails))
    deriving (Generic, B.Beamable)
  primaryKey t = RiderDetailsPrimaryKey t.id

deriveTableEncryption ''RiderDetailsTE

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RiderDetailsT)
fieldEMod =
  B.setEntityName "rider_details"
    <> B.modifyTableFields
      (B.tableModification @_ @RiderDetailsT)
        { mobileCountryCode = "mobile_country_code",
          mobileNumber =
            EncryptedHashed
              { encrypted = "mobile_number_encrypted",
                hash = "mobile_number_hash"
              },
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
