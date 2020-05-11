module Storage.Queries.Location where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Beckn.Types.App
import Beckn.Utils.Extra
import qualified Types.Storage.DB as DB
import qualified Beckn.Types.Storage.Location as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.transporterDb

create :: Storage.Location -> L.Flow ()
create Storage.Location {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Location {..})
    >>= either DB.throwDBError pure

findLocationById ::
  LocationId -> L.Flow Storage.Location
findLocationById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_DATA"
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)

updateLocationRec :: LocationId -> Storage.Location -> L.Flow ()
updateLocationRec locationId location = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause location now) (predicate locationId)
    >>= either DB.throwDBError pure
  where
    setClause location n Storage.Location {..} =
      mconcat
        [ _locationType <-. B.val_ (Storage._locationType  location)
        , _lat <-. B.val_ (Storage._lat  location)
        , _long <-. B.val_ (Storage._long  location)
        , _ward <-. B.val_ (Storage._ward  location)
        , _district <-. B.val_ (Storage._district  location)
        , _city <-. B.val_ (Storage._city  location)
        , _state <-. B.val_ (Storage._state  location)
        , _country <-. B.val_ (Storage._country  location)
        , _pincode <-. B.val_ (Storage._pincode  location)
        , _address <-. B.val_ (Storage._address  location)
        , _bound <-. B.val_ (Storage._bound  location)
        , _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Location {..} = _id ==. B.val_ id


findAllByLocIds :: [Text] ->  L.Flow [Storage.Location]
findAllByLocIds ids =
  DB.findAllOrErr dbTable (pred (LocationId <$> ids))
  where
    pred ids Storage.Location {..} =
     B.in_ _id (B.val_ <$> ids)