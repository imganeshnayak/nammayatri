{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ProductInstance where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.SearchReqLocation as Loc
import Types.API.ProductInstance
import qualified Types.Storage.Case as Case
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import qualified Types.Storage.ProductInstance as Storage
import Types.Storage.Products
import qualified Types.Storage.Products as Product
import qualified Types.Storage.SearchReqLocation as Loc
import Types.Storage.Vehicle (Vehicle)

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT))
getDbTable = DB.productInstance . DB.transporterDb <$> getSchemaName

getCsTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Case.CaseT))
getCsTable =
  DB._case . DB.transporterDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.ProductInstance -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.ProductInstance -> DB.SqlDB ()
create productInstance = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue productInstance)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.ProductInstance]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ createdAt
    predicate Storage.ProductInstance {..} =
      B.in_ productId (B.val_ <$> ids)

findById' :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById' productInstanceId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      id ==. B.val_ productInstanceId

findAllByIds' :: DBFlow m r => [Id Storage.ProductInstance] -> m [Storage.ProductInstance]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusFlow prodInstId status = DB.runSqlDB (updateStatus prodInstId status)

updateStatus ::
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatus prodInstId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      id ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateCaseId ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Id Case.Case ->
  m ()
updateCaseId prodInstId caseId_ = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update
    dbTable
    (setClause caseId_ currTime)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause scCaseId currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          caseId <-. B.val_ scCaseId
        ]

updateDistance ::
  Id Person ->
  Double ->
  DB.SqlDB ()
updateDistance driverId distance' = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause distance')
    (predicate driverId)
  where
    predicate driverId' Storage.ProductInstance {..} =
      personId ==. B.val_ (Just driverId')
        &&. status ==. B.val_ Storage.INPROGRESS
    setClause distance'' Storage.ProductInstance {..} =
      traveledDistance <-. B.current_ traveledDistance + B.val_ distance''

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

productInstancejoinQuery ::
  ( B.Database be db,
    B.HasSqlEqualityCheck be (Id Case.Case),
    B.HasSqlEqualityCheck be (Id Product)
  ) =>
  B.DatabaseEntity be db (B.TableEntity Case.CaseT) ->
  B.DatabaseEntity be db (B.TableEntity ProductsT) ->
  B.DatabaseEntity be db (B.TableEntity Storage.ProductInstanceT) ->
  (Case.CaseT (B.QExpr be s) -> B.QExpr be s Bool) ->
  (ProductsT (B.QExpr be s) -> B.QExpr be s Bool) ->
  (Storage.ProductInstanceT (B.QExpr be s) -> B.QExpr be s Bool) ->
  B.Q
    be
    db
    s
    ( Case.CaseT (B.QExpr be s),
      ProductsT (B.QExpr be s),
      Storage.ProductInstanceT (B.QExpr be s)
    )
productInstancejoinQuery tbl1 tbl2 tbl3 pred1 pred2 pred3 = do
  i <- B.filter_ pred1 $ B.all_ tbl1
  j <- B.filter_ pred2 $ B.all_ tbl2
  k <- B.filter_ pred3 $
    B.join_ tbl3 $
      \line ->
        Case.CasePrimaryKey (Storage.caseId line) B.==. B.primaryKey i
          B.&&. ProductsPrimaryKey (Storage.productId line) B.==. B.primaryKey j
  pure (i, j, k)

productInstanceJoin :: DBFlow m r => Int -> Int -> [Case.CaseType] -> Id Org.Organization -> [Storage.ProductInstanceStatus] -> m ProductInstanceList
productInstanceJoin limit_ offset_ csTypes orgId status_ = do
  dbTable <- getDbTable
  prodTable <- getProdTable
  csTable <- getCsTable
  joinedValues <-
    DB.findAllByJoin
      (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
      (productInstancejoinQuery csTable prodTable dbTable csPred prodPred piPred)
  return $ mkJoinRes <$> joinedValues
  where
    limit = toInteger limit_
    offset = toInteger offset_
    orderByDesc (_, _, Storage.ProductInstance {..}) = B.desc_ createdAt
    csPred Case.Case {..} =
      _type `B.in_` (B.val_ <$> csTypes) ||. complementVal csTypes
    prodPred Product.Products {..} = B.val_ True
    piPred Storage.ProductInstance {..} =
      organizationId ==. B.val_ orgId
        &&. (status `B.in_` (B.val_ <$> status_) ||. complementVal status_)
    mkJoinRes (cs, pr, cpr) =
      ProductInstanceRes
        { _case = cs,
          product = pr,
          productInstance = cpr,
          fromLocation = Nothing,
          toLocation = Nothing
        }

findById :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} = id ==. B.val_ pid

updateDriver ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Person) ->
  DB.SqlDB ()
updateDriver ids driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause driverId now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.ProductInstance {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateVehicle ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Vehicle) ->
  DB.SqlDB ()
updateVehicle ids vehId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause (getId <$> vehId) now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime' Storage.ProductInstance {..} =
      mconcat
        [ entityId <-. B.val_ vehicleId,
          updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.ProductInstance -> Text -> DB.SqlDB ()
updateInfo prodInstId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause pInfo Storage.ProductInstance {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

updateActualPrice :: Amount -> Id Storage.ProductInstance -> DB.SqlDB ()
updateActualPrice price' prodInstId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.ProductInstance {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

updateTotalFare :: Amount -> Id Storage.ProductInstance -> DB.SqlDB ()
updateTotalFare totalFare' prodInstId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause totalFare' now)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause totalFare_ currTime Storage.ProductInstance {..} =
      mconcat
        [ totalFare <-. B.val_ (Just totalFare_),
          updatedAt <-. B.val_ currTime
        ]

findAllByVehicleId :: DBFlow m r => Maybe (Id Vehicle) -> m [Storage.ProductInstance]
findAllByVehicleId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust piId) &&. entityId ==. B.val_ (getId <$> piId)

findAllRidesWithLocationsByDriverId ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Id Person ->
  m [(Storage.ProductInstance, Loc.SearchReqLocation, Loc.SearchReqLocation)]
findAllRidesWithLocationsByDriverId limit offset personId_ = do
  piTable <- getDbTable
  locTable <- Loc.getDbTable
  DB.findAllByJoin
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    (joinQuery piTable locTable)
  where
    joinQuery piTable locTable = do
      ride <- B.filter_ predicate $ B.all_ piTable
      fromLoc <- B.join_ locTable $ \loc -> B.just_ loc.id ==. ride.fromLocation
      toLoc <- B.join_ locTable $ \loc -> B.just_ loc.id ==. ride.toLocation
      return (ride, fromLoc, toLoc)
    predicate Storage.ProductInstance {..} =
      personId ==. B.val_ (Just personId_)
        &&. _type ==. B.val_ Case.RIDEORDER
    orderByDesc (Storage.ProductInstance {..}, _, _) = B.desc_ createdAt

findAllByParentId :: DBFlow m r => Id Storage.ProductInstance -> m [Storage.ProductInstance]
findAllByParentId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = parentId ==. B.val_ (Just piId)

findOrderPIByParentId :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findOrderPIByParentId pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate piid Storage.ProductInstance {..} =
      parentId ==. B.val_ (Just piid)
        &&. _type ==. B.val_ Case.RIDEORDER

findByIdType :: DBFlow m r => [Id Storage.ProductInstance] -> Case.CaseType -> m (Maybe Storage.ProductInstance)
findByIdType ids csType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      id `B.in_` (B.val_ <$> ids)
        &&. _type ==. B.val_ csType

findByParentIdType :: DBFlow m r => Id Storage.ProductInstance -> Case.CaseType -> m (Maybe Storage.ProductInstance)
findByParentIdType mparentId csType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      parentId ==. B.val_ (Just mparentId)
        &&. _type ==. B.val_ csType

getCountRideOrder :: DBFlow m r => Id Org.Organization -> m [(Storage.ProductInstanceStatus, Int)]
getCountRideOrder orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.aggregate_ aggregator) predicate
  where
    aggregator Storage.ProductInstance {..} = (B.group_ status, B.as_ @Int B.countAll_)
    predicate Storage.ProductInstance {..} =
      organizationId ==. B.val_ orgId
        &&. _type ==. B.val_ Case.RIDEORDER
