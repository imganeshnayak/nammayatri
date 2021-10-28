module Storage.Queries.DriverInformation where

import Beckn.External.Encryption (getDbHash)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QPerson
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverInformation as DriverInformation
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Person

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity DriverInformation.DriverInformationT))
getDbTable = DB.driverInformation . DB.transporterDb <$> getSchemaName

create :: DriverInformation.DriverInformation -> DB.SqlDB ()
create DriverInformation.DriverInformation {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue DriverInformation.DriverInformation {..})

findById :: DBFlow m r => Id Driver -> m (Maybe DriverInformation.DriverInformation)
findById driverId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    personId_ = cast driverId_
    predicate DriverInformation.DriverInformation {..} = driverId ==. B.val_ personId_

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

fetchAllAvailableByIds :: DBFlow m r => [Id Driver] -> m [DriverInformation.DriverInformation]
fetchAllAvailableByIds driversIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    personsIds = cast <$> driversIds
    predicate DriverInformation.DriverInformation {..} =
      foldr
        (&&.)
        (B.val_ True)
        [ driverId `B.in_` (B.val_ <$> personsIds),
          active ==. B.val_ True,
          onRide ==. B.val_ False
        ]

updateActivity :: Id Driver -> Bool -> DB.SqlDB ()
updateActivity driverId_ isActive = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause isActive now) (predicate personId_)
  where
    personId_ = cast driverId_
    setClause activityValue now DriverInformation.DriverInformation {..} =
      mconcat
        [ active <-. B.val_ activityValue,
          updatedAt <-. B.val_ now
        ]
    predicate id DriverInformation.DriverInformation {..} = driverId ==. B.val_ id

updateEnabledState :: Id Driver -> Bool -> DB.SqlDB ()
updateEnabledState driverId_ isEnabled = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause isEnabled now) (predicate personId_)
  where
    personId_ = cast driverId_
    setClause enabledValue now DriverInformation.DriverInformation {..} =
      mconcat
        [ enabled <-. B.val_ enabledValue,
          updatedAt <-. B.val_ now
        ]
    predicate id DriverInformation.DriverInformation {..} = driverId ==. B.val_ id

updateOnRideFlow :: DBFlow m r => Id Driver -> Bool -> m ()
updateOnRideFlow driverId onRide =
  DB.runSqlDB (updateOnRide driverId onRide)

updateOnRide ::
  Id Driver ->
  Bool ->
  DB.SqlDB ()
updateOnRide driverId_ onRide_ = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause onRide_ now) (predicate personId_)
  where
    personId_ = cast driverId_
    setClause onR now' DriverInformation.DriverInformation {..} =
      mconcat
        [ onRide <-. B.val_ onR,
          updatedAt <-. B.val_ now'
        ]
    predicate id DriverInformation.DriverInformation {..} = driverId ==. B.val_ id

deleteById :: Id Driver -> DB.SqlDB ()
deleteById driverId_ = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate personId_)
  where
    personId_ = cast driverId_
    predicate pid DriverInformation.DriverInformation {..} = driverId ==. B.val_ pid

findAllWithLimitOffsetByOrgId ::
  (DBFlow m r, EncFlow m r) =>
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  Id Org.Organization ->
  m [(Person.Person, DriverInformation.DriverInformation)]
findAllWithLimitOffsetByOrgId mbSearchString mbLimit mbOffset orgId = do
  personDbTable <- QPerson.getDbTable
  driverInfoDbTable <- getDbTable
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  DB.findAllByJoin (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) (joinQuery personDbTable driverInfoDbTable mbSearchStrDBHash)
  where
    orderByDesc (Person.Person {..}, _) = B.desc_ createdAt
    limit = fromMaybe 100 mbLimit
    offset = fromMaybe 0 mbOffset
    joinQuery personDbTable driverInfoDbTable mbSearchStrDBHash = do
      person <- B.all_ personDbTable
      driverInfo <- B.join_ driverInfoDbTable $ \row -> do
        row.driverId B.==. person.id
      B.guard_ $ predicate mbSearchStrDBHash person
      return (person, driverInfo)

    predicate mbSearchStrDBHash p@Person.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ role B.==. B.val_ Person.DRIVER,
          organizationId B.==. B.val_ (Just orgId),
          maybe (B.val_ True) (filterBySearchString p) $ liftA2 (,) mbSearchString mbSearchStrDBHash
        ]

    filterBySearchString Person.Person {..} (searchStr, searchStrDBHash) = do
      let likeSearchStr = B.concat_ [B.val_ "%", B.val_ searchStr, B.val_ "%"]
      B.lower_ (B.concat_ [unMaybe firstName, B.val_ " ", unMaybe middleName, B.val_ " ", unMaybe lastName]) `B.like_` B.lower_ likeSearchStr
        B.||. mobileNumber.hash B.==. B.val_ (Just searchStrDBHash)
    unMaybe = B.maybe_ (B.val_ "") identity
