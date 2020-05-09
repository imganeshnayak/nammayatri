module Types.API.External.Mobility.Stop where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Core.Location

data Stop =
  Stop
    { _location :: Location
    , _arrival_time :: LocalTime
    , _departure_time :: LocalTime
    }
      deriving (Generic, Show)

instance FromJSON Stop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Stop where
  toJSON = genericToJSON stripLensPrefixOptions
