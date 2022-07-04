{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.RentalFarePolicy as Domain
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Quote.OneWayQuote
import Storage.Tabular.Quote.RentalQuote
import Storage.Tabular.Quote.Table
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyT)
import Storage.Tabular.Vehicle ()

data QuoteDetailsT = OneWayDetailsT OneWayQuoteT | RentalDetailsT (RentalQuoteT, RentalFarePolicyT)

type FullQuoteT = (QuoteT, QuoteDetailsT)

instance TType FullQuoteT Domain.Quote where
  fromTType (QuoteT {..}, tQuoteDetails) = do
    quoteDetails <- case tQuoteDetails of
      RentalDetailsT (_, rental) -> do
        Domain.RentalDetails <$> fromTType rental
      OneWayDetailsT oneWay -> do
        let oneWayQuoteEntity = oneWayQuoteFromTType oneWay
        return . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distance = oneWayQuoteEntity.distance,
              distanceToNearestDriver = oneWayQuoteEntity.distance,
              ..
            }
    pure
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          productId = fromKey productId,
          providerId = fromKey providerId,
          ..
        }

  toTType Domain.Quote {..} = do
    let details = case quoteDetails of
          Domain.OneWayDetails oneWay -> OneWayDetailsT $ oneWayQuoteToTType id oneWay
          Domain.RentalDetails rental -> do
            let rentalQuoteT = rentalToTType id rental.id
                rentalFarePolicyT = toTType rental
            RentalDetailsT (rentalQuoteT, rentalFarePolicyT)
    let quoteT =
          QuoteT
            { id = getId id,
              fareProductType = Domain.getFareProductType quoteDetails,
              requestId = toKey requestId,
              productId = toKey productId,
              providerId = toKey providerId,
              ..
            }
    (quoteT, details)

oneWayQuoteFromTType :: OneWayQuoteT -> Domain.OneWayQuoteDetails
oneWayQuoteFromTType OneWayQuoteT {..} = do
  Domain.OneWayQuoteDetails
    { distance = HighPrecMeters distance,
      distanceToNearestDriver = HighPrecMeters distanceToNearestDriver,
      ..
    }

oneWayQuoteToTType :: Id Domain.Quote -> Domain.OneWayQuoteDetails -> OneWayQuoteT
oneWayQuoteToTType quoteId Domain.OneWayQuoteDetails {..} =
  OneWayQuoteT
    { quoteId = toKey quoteId,
      distance = getHighPrecMeters distance,
      distanceToNearestDriver = getHighPrecMeters distanceToNearestDriver,
      ..
    }

rentalToTType :: Id Domain.Quote -> Id Domain.RentalFarePolicy -> RentalQuoteT
rentalToTType quoteId rentalFarePolicyId =
  RentalQuoteT
    { quoteId = toKey quoteId,
      rentalFarePolicyId = toKey rentalFarePolicyId
    }
