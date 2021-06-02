module Fixtures.ProductInstance where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultProductInstance :: ProductInstance.ProductInstance
defaultProductInstance =
  ProductInstance.ProductInstance
    { id = Id "1",
      caseId = Id "1",
      productId = Id "1",
      personId = Just $ Id "1",
      personUpdatedAt = Nothing,
      shortId = "",
      entityType = ProductInstance.VEHICLE,
      entityId = Nothing,
      quantity = 0,
      price = Just . Amount.Amount $ 100 % 1,
      _type = Case.RIDEORDER,
      status = ProductInstance.COMPLETED,
      startTime = Fixtures.defaultTime,
      endTime = Nothing,
      validTill = Fixtures.defaultTime,
      fromLocation = Nothing,
      toLocation = Nothing,
      organizationId = "",
      parentId = Nothing,
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      info = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
