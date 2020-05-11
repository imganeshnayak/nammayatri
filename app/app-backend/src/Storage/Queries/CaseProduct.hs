module Storage.Queries.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.CaseProduct as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseProductT)
dbTable = DB._caseProduct DB.appDb

create :: Storage.CaseProduct -> L.Flow ()
create Storage.CaseProduct {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CaseProduct {..})
    >>= either DB.throwDBError pure

findAllByIds :: Integer -> Integer -> [ProductsId] -> L.Flow [Storage.CaseProduct]
findAllByIds limit offset ids =
  DB.findAllWithLimitOffsetWhere dbTable (pred ids) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.CaseProduct {..} = B.desc_ _createdAt
    pred ids Storage.CaseProduct {..} =
      B.in_ _productId (B.val_ <$> ids)
