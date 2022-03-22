module Fixtures.Quote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultQuote :: Quote.Quote
defaultQuote =
  Quote.Quote
    { id = Id "1",
      requestId = Id "1",
      productId = Id "1",
      estimatedFare = 0,
      estimatedTotalFare = 0,
      discount = Nothing,
      distance = 0,
      providerId = "",
      distanceToNearestDriver = 0,
      vehicleVariant = Veh.SUV,
      createdAt = Fixtures.defaultTime
    }
