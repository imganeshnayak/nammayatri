{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Ticket where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.Ticket as Domain
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH (enableKVPG, mkTableInstances)

data TicketT f = TicketT
  { id :: B.C f Text,
    status :: B.C f Domain.TicketStatus,
    quoteId :: B.C f (Maybe Text),
    searchRequestId :: B.C f Text,
    bppOrderId :: B.C f (Maybe Text),
    itemId :: B.C f Text,
    bppTicketId :: B.C f (Maybe Text),
    fulfillmentId :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    quantity :: B.C f Integer,
    fromLocationId :: B.C f Text,
    pricePerAdult :: B.C f Money,
    totalPrice :: B.C f Money,
    qrData :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketT where
  data PrimaryKey TicketT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Ticket = TicketT Identity

$(enableKVPG ''TicketT ['id] [])

$(mkTableInstances ''TicketT "ticket")
