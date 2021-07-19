{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Products as Product
import qualified Types.Storage.Ride as Storage
import qualified Types.Storage.SearchRequest as SearchRequest

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RideT))
getDbTable = DB.ride . DB.appDb <$> getSchemaName

getPITable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity PI.ProductInstanceT))
getPITable = DB.productInstance . DB.appDb <$> getSchemaName

getSRTable :: DBFlow m r => m (B.DatabaseEntity be DB.AppDb (B.TableEntity SearchRequest.SearchRequestT))
getSRTable =
  DB.searchRequest . DB.appDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Ride -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Ride -> DB.SqlDB ()
create ride = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue ride)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.Ride]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Ride {..} = B.desc_ createdAt
    predicate Storage.Ride {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Ride]
findAllByRequestId reqId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = requestId ==. B.val_ reqId

findByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m (Maybe Storage.Ride)
findByRequestId reqId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = requestId ==. B.val_ reqId

findAllByRequestId' :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Ride]
findAllByRequestId' searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      requestId ==. B.val_ searchRequestId

findAllByIds' :: DBFlow m r => [Id Storage.Ride] -> m [Storage.Ride]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusForProducts :: DBFlow m r => Id Product.Products -> Storage.RideStatus -> m ()
updateStatusForProducts productId_ status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate productId_)
  where
    predicate pId Storage.Ride {..} = productId ==. B.val_ pId
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.Ride ->
  Storage.RideStatus ->
  m ()
updateStatusFlow prodInstId status = DB.runSqlDB (updateStatus prodInstId status)

updateStatus ::
  Id Storage.Ride ->
  Storage.RideStatus ->
  DB.SqlDB ()
updateStatus prodInstId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.Ride {..} =
      id ==. B.val_ pId
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

findAllByRequestIds :: DBFlow m r => [Id SearchRequest.SearchRequest] -> m [Storage.Ride]
findAllByRequestIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ requestId (B.val_ <$> ids)

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.Ride] ->
  Storage.RideStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.Ride] ->
  Storage.RideStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.Ride {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateRequestId ::
  DBFlow m r =>
  Id Storage.Ride ->
  Id SearchRequest.SearchRequest ->
  m ()
updateRequestId prodInstId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate prodInstId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause scRequestId currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ scRequestId
        ]

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.Ride]
findAllByProdId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = productId ==. B.val_ piId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findById :: DBFlow m r => Id Storage.Ride -> m (Maybe Storage.Ride)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = id ==. B.val_ pid

updateDriverFlow :: DBFlow m r => Id Storage.Ride -> Maybe (Id Person) -> m ()
updateDriverFlow rideId driverId =
  DB.runSqlDB (updateDriver rideId driverId)

updateDriver ::
  Id Storage.Ride ->
  Maybe (Id Person) ->
  DB.SqlDB ()
updateDriver rideId driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause driverId now)
    (predicate rideId)
  where
    predicate rideId' Storage.Ride {..} = id B.==. B.val_ rideId'
    setClause sDriverId currTime Storage.Ride {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateInfo :: Id Storage.Ride -> Text -> DB.SqlDB ()
updateInfo prodInstId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate prodInstId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause pInfo Storage.Ride {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

findAllByPersonId :: DBFlow m r => Id Person -> m [Storage.Ride]
findAllByPersonId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = personId ==. B.val_ (Just piId)

findByProductInstanceId :: DBFlow m r => Id PI.ProductInstance -> m (Maybe Storage.Ride)
findByProductInstanceId prodInstId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} =
      productInstanceId ==. B.val_ prodInstId

findAllExpiredByStatus :: DBFlow m r => [Storage.RideStatus] -> UTCTime -> m [Storage.Ride]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

getCountByStatus :: DBFlow m r => Id Org.Organization -> m [(Storage.RideStatus, Int)]
getCountByStatus orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.aggregate_ aggregator) predicate
  where
    aggregator Storage.Ride {..} = (B.group_ status, B.as_ @Int B.countAll_)
    predicate Storage.Ride {..} =
      organizationId ==. B.val_ orgId

findByStartTimeBuffer ::
  DBFlow m r =>
  UTCTime ->
  NominalDiffTime ->
  [Storage.RideStatus] ->
  m [Storage.Ride]
findByStartTimeBuffer startTime_ buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime_
  let toTime = addUTCTime (buffer * 60 * 60) startTime_
  DB.findAll dbTable identity (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.Ride {..} =
      let inStatus = fmap B.val_ statuses
       in startTime B.<=. B.val_ toTime
            &&. startTime B.>=. B.val_ fromTime
            &&. status `B.in_` inStatus

getDriverCompletedRides :: DBFlow m r => Id Person -> UTCTime -> UTCTime -> m [Storage.Ride]
getDriverCompletedRides driverId fromTime toTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      personId ==. B.val_ (Just driverId)
        &&. status ==. B.val_ Storage.COMPLETED
        &&. startTime B.>=. B.val_ fromTime
        &&. startTime B.<=. B.val_ toTime

getFullRideInfo ::
  DBFlow m r =>
  Int ->
  Int ->
  Id Org.Organization ->
  [Storage.RideStatus] ->
  m [(SearchRequest.SearchRequest, Product.Products, PI.ProductInstance, Storage.Ride)]
getFullRideInfo limit_ offset_ orgId status_ = do
  prodInstTable <- getPITable
  prodTable <- getProdTable
  srTable <- getSRTable
  rideTable <- getDbTable
  DB.findAllByJoin
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    (rideJoinQuery srTable prodTable prodInstTable rideTable)
  where
    limit = toInteger limit_
    offset = toInteger offset_
    orderByDesc (_, _, _, Storage.Ride {..}) = B.desc_ createdAt
    ridePred Storage.Ride {..} =
      organizationId ==. B.val_ orgId
        &&. (status `B.in_` (B.val_ <$> status_) ||. complementVal status_)
    rideJoinQuery searchRequestTable productsTable prodInstTable rideTable = do
      searchRequest <- B.all_ searchRequestTable
      products <- B.all_ productsTable
      prodInst <- B.all_ prodInstTable
      ride <- B.filter_ ridePred $
        B.join_ rideTable $
          \line ->
            SearchRequest.SearchRequestPrimaryKey (Storage.requestId line) B.==. B.primaryKey searchRequest
              B.&&. Product.ProductsPrimaryKey (Storage.productId line) B.==. B.primaryKey products
              B.&&. PI.ProductInstancePrimaryKey (Storage.productInstanceId line) B.==. B.primaryKey prodInst
      pure (searchRequest, products, prodInst, ride)

updateActualPrice :: Amount -> Id Storage.Ride -> DB.SqlDB ()
updateActualPrice price' rideId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate rideId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.Ride {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

findAllByPerson :: DBFlow m r => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.Ride]
findAllByPerson perId mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset) $ predicate isOnlyActive
  where
    predicate isOnlyActive Storage.Ride {..} =
      personId ==. B.val_ (Just perId)
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True

updateMultipleFlow ::
  DBFlow m r =>
  Id Storage.Ride ->
  Storage.Ride ->
  m ()
updateMultipleFlow id ride = DB.runSqlDB (updateMultiple id ride)

updateMultiple :: Id Storage.Ride -> Storage.Ride -> DB.SqlDB ()
updateMultiple rideId ride = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update' dbTable (setClause currTime ride) (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause now ride_ Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (ride_.status),
          --personId <-. B.val_ (Storage.personId prd),
          fromLocation <-. B.val_ (ride_.fromLocation),
          toLocation <-. B.val_ (ride_.toLocation),
          info <-. B.val_ (ride_.info),
          udf4 <-. B.val_ (ride_.udf4),
          actualPrice <-. B.val_ (ride_.actualPrice),
          actualDistance <-. B.val_ (ride_.actualDistance)
        ]