module App
  ( runMockApp,
  )
where

import App.Server
import App.Types
import qualified Beckn.Types.App as App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server (exceptionResponse)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Runtime as E
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )

runMockApp :: IO ()
runMockApp = do
  appEnv <- readDhallConfigDefault tyEnv "mock-app-backend"
  Metrics.serve (metricsPort appEnv)
  let loggerCfg = getEulerLoggerConfig "/tmp/mock-app-backend.log" $ loggerConfig appEnv
  let settings =
        setOnExceptionResponse mockAppExceptionResponse $
          setPort (port appEnv) defaultSettings
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    reqHeadersKey <- V.newKey
    _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
    runSettings settings $ run reqHeadersKey (App.EnvR flowRt appEnv)

mockAppExceptionResponse :: SomeException -> Response
mockAppExceptionResponse = exceptionResponse
