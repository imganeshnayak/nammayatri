{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.Quote.Table

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalQuoteT sql=rental_quote
      quoteId QuoteTId
      baseDistance Int
      baseDuration Int
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey RentalQuoteT where
  type DomainKey RentalQuoteT = Id Domain.Quote
  fromKey (RentalQuoteTKey _id) = fromKey _id
  toKey id = RentalQuoteTKey $ toKey id
