{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.AllocationEvent where

import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)
import Types.App (Driver)

data AllocationEvent = AllocationEvent
  { id :: Id AllocationEvent,
    driverId :: Maybe (Id Driver),
    eventType :: AllocationEventType,
    timestamp :: UTCTime,
    rideBookingId :: Id DRB.RideBooking
  }
  deriving (Generic)

data AllocationEventType
  = NotificationSent
  | MarkedAsAccepted
  | MarkedAsRejected
  | MarkedAsIgnored
  | AcceptedByDriver
  | RejectedByDriver
  | ConsumerCancelled
  | EmptyDriverPool
  | AllocationTimeFinished
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)
