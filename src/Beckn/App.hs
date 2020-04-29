module Beckn.App where

import qualified Beckn.App.Server             as App
import           Beckn.Constants.APIErrorCode
import           Beckn.Storage.DB.Config
import qualified Beckn.Types.App              as App
import           Beckn.Utils.Storage
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Char8        as BS
import qualified Data.Vault.Lazy              as V
import qualified EulerHS.Interpreters         as R
import qualified EulerHS.Language             as L
import           EulerHS.Prelude
import qualified EulerHS.Runtime              as R
import qualified EulerHS.Types                as T
import qualified Network.HTTP.Client          as Client
import qualified Network.HTTP.Types           as H
import           Network.Wai
import           Network.Wai.Handler.Warp     (Settings, defaultSettings, run,
                                               runSettings,
                                               setOnExceptionResponse, setPort)
import           Servant
import           Servant.Server
import qualified System.Environment           as SE

runBecknBackendApp :: IO ()
runBecknBackendApp = do
  port <- fromMaybe 8012 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  runBecknBackendApp' port $
    setOnExceptionResponse becknExceptionResponse $
    setPort port defaultSettings

runBecknBackendApp' :: Int -> Settings -> IO ()
runBecknBackendApp' port settings = do
  reqHeadersKey <- V.newKey
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True
          , T._logFilePath = "/tmp/newton-backend.log"
          , T._isAsync = False
          }
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections
    try (R.runFlow flowRt prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String
          ("Runtime created. Starting server at port " <> show port)
        runSettings settings $
          App.run reqHeadersKey $
          App.Env flowRt

becknExceptionResponse :: SomeException -> Response
becknExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) (BS.pack $ errReasonPhrase ex))
        ((H.hContentType, "application/json") : (errHeaders ex))
        (errBody ex)
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode $ internalServerErr)


