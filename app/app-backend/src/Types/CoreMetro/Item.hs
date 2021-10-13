module Types.CoreMetro.Item where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price,
    location_id :: Text,
    stops :: [Stop]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Stop = Stop
  { id :: Text,
    time :: ScheduleObj
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype ScheduleObj = ScheduleObj {schedule :: TimeObj}
  deriving (Generic, FromJSON, ToJSON, Show)

newtype TimeObj = TimeObj {times :: [UTCTime]}
  deriving (Generic, FromJSON, ToJSON, Show)
