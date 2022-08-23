module Domain.Types.Ride.Type where

import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Person as DPers
import EulerHS.Prelude hiding (id)
import Servant.API

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    totalFare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
