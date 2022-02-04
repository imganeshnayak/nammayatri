module Core.Search.LocationGps where

import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.GenericPretty (PrettyShow)
import Data.Aeson
import Relude hiding (id)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)
