{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Booking where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Quote (Quote)
import Domain.Search (Person, Search)

data BookingStatus = NEW | AWAITING_PAYMENT | CONFIRMED | CANCELLED
  deriving (Generic, Show, Read)

data Booking = Booking
  { id :: Id Booking,
    searchId :: Id Search,
    quoteId :: Id Quote,
    requestorId :: Id Person,
    requestorNumber :: Text,
    vehicleNumber :: Text,
    additionalInfo :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingSpaceLocationId :: Text,
    parkingSupportNumber :: Text,
    fare :: Amount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    status :: BookingStatus,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
