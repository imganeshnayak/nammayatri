{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable ::
  Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RegistrationTokenT))
getDbTable =
  DB.registrationToken . DB.appDb <$> getSchemaName

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})

findById :: Text -> Flow (Maybe Storage.RegistrationToken)
findById rtId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RegistrationToken {..} = id ==. B.val_ rtId

findByToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findByToken token_ = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate token_)
  where
    predicate rtoken Storage.RegistrationToken {..} = token ==. B.val_ rtoken

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps rtId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause attemps now) (predicate rtId)
  findById rtId >>= fromMaybeM (TokenNotFound rtId)
  where
    predicate i Storage.RegistrationToken {..} = id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [attempts <-. B.val_ a, updatedAt <-. B.val_ n]

deleteByPersonId :: Text -> Flow ()
deleteByPersonId rtId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rtId)
  where
    predicate rtid Storage.RegistrationToken {..} = entityId ==. B.val_ rtid
