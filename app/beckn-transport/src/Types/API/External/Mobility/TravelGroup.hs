module Types.API.External.Mobility.TravelGroup where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Core.Person

data TravelGroup =
  TravelGroup
    { _primary_traveller :: Person -- "PULL", "PUSH"
    , _group_size :: Int
    }
      deriving (Generic, Show)

instance FromJSON TravelGroup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TravelGroup where
  toJSON = genericToJSON stripLensPrefixOptions