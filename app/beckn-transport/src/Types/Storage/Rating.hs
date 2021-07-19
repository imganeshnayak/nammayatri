{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Rating where

import Beckn.Types.Id
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.Person (Person)
import Types.Storage.Ride (Ride)

data RatingT f = Rating
  { id :: B.C f (Id Rating),
    rideId :: B.C f (Id Ride),
    driverId :: B.C f (Id Person),
    ratingValue :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Rating = RatingT Identity

type RatingPrimaryId = B.PrimaryKey RatingT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table RatingT where
  data PrimaryKey RatingT f = RatingPrimaryKey (B.C f (Id Rating))
    deriving (Generic, B.Beamable)
  primaryKey = RatingPrimaryKey . id

deriving instance Show Rating

deriving instance Eq Rating

deriving instance FromJSON Rating

deriving instance ToJSON Rating

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RatingT)
fieldEMod =
  B.setEntityName "rating"
    <> B.modifyTableFields
      B.tableModification
        { rideId = "ride_id",
          ratingValue = "rating_value",
          driverId = "driver_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
