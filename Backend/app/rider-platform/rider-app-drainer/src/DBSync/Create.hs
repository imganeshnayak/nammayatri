module DBSync.Create where

import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text as T hiding (any, map, null)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import EulerHS.Language as EL
import EulerHS.Prelude
import Text.Casing (pascal)
import "rider-app" Tools.Beam.UtilsTH (currentSchemaName)
import Types.DBSync
import Utils.Utils

-- import qualified Data.ByteString as BS
-- import Kernel.Prelude (UTCTime)
-- import qualified Data.Time.Clock.POSIX as Time
-- import System.FilePath.Posix as Path
-- import qualified System.Directory as Dir

-- | This function is used to run the create operation for a single entry in the stream
runCreate :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreate createDataEntry streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = createDataEntry
  EL.logDebug ("BYTE STRING" :: Text) (show streamData)
  case A.eitherDecode @DBCreateObject . LBS.fromStrict $ streamData of
    Right createDBModel -> do
      EL.logDebug ("DB OBJECT" :: Text) (show createDBModel)
      let tableName = createDBModel.dbModel
      -- uncomment for debug purposes
      -- writeDebugFile tableName entryId "streamData.json" streamData
      -- writeDebugFile tableName entryId "dbObject.txt" $ show createDBModel
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runCreateQuery createDataEntry createDBModel
        else do
          let createObject = getCreateObjectForKafka tableName createDBModel.contentsObj
          res <- EL.runIO $ createInKafka _kafkaConnection createObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA CREATE FAILED" :: Text) (err <> " for Object :: " <> show createDBModel.contents)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA CREATE SUCCESSFUL" :: Text) (" Create successful for object :: " <> show createDBModel.contents)
              runCreateQuery createDataEntry createDBModel
    Left err -> do
      EL.logError ("CREATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData <> "; error :: " <> show err)
      return $ Left entryId

-- | Run a create query for a single entry in the stream
runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBCreateObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreateQuery createDataEntry dbCreateObject = do
  Env {..} <- ask
  let (entryId, byteString) = createDataEntry
      dbModel = dbCreateObject.dbModel
  if shouldPushToKafkaOnly dbModel _dontEnableDbTables
    then return $ Right entryId
    else do
      let insertQuery = generateInsertForTable dbCreateObject
      case insertQuery of
        Just query -> do
          EL.logDebug ("QUERY" :: Text) query -- TODO redundant
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY INSERT FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              -- uncomment for debug purposes
              -- writeDebugFile dbModel entryId "queryFailed.sql" $ encodeUtf8 query
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("QUERY INSERT SUCCESSFUL" :: Text) (" Insert successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              -- uncomment for debug purposes
              -- writeDebugFile dbModel entryId "querySuccessful.sql" $ encodeUtf8 query
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

-- | Generate an insert query for the rider_app schema
generateInsertForTable :: DBCreateObject -> Maybe Text
generateInsertForTable DBCreateObject {dbModel, contents, mappings} = do
  let DBCreateObjectContent termWarps = contents
  let schema = SchemaName $ T.pack currentSchemaName
  generateInsertQuery InsertQuery {..}

getCreateObjectForKafka :: DBModel -> A.Object -> A.Value
getCreateObjectForKafka model content =
  A.object
    [ "contents" A..= content,
      "tag" A..= ((T.pack . pascal . T.unpack) model.getDBModel <> "Object"),
      "type" A..= ("INSERT" :: Text)
    ]

-- uncomment for debug purposes
-- writeDebugFile ::
--   DBModel ->
--   EL.KVDBStreamEntryID ->
--   String ->
--   BS.ByteString ->
--   ReaderT Env EL.Flow ()
-- writeDebugFile dbModel entryId fileName entity = EL.runIO $ do
--   let fullPath'Name = "/tmp/drainer/rider/create/" <> T.unpack dbModel.getDBModel <> "/" <> show (mkTimeStamp entryId) <> "/" <> fileName
--   let fullPath = Path.takeDirectory fullPath'Name
--   Dir.createDirectoryIfMissing True fullPath
--   BS.writeFile fullPath'Name entity

-- mkTimeStamp :: EL.KVDBStreamEntryID -> UTCTime
-- mkTimeStamp (EL.KVDBStreamEntryID posixTime _) = Time.posixSecondsToUTCTime $ fromInteger (posixTime `div` 1000)
