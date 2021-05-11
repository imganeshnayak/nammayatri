{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import System.Environment

runFMDWrapper :: (AppCfg -> AppCfg) -> IO ()
runFMDWrapper configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg ^. #loggerConfig
  let settings = setPort (appCfg ^. #port) defaultSettings
  appEnv <- buildAppEnv appCfg
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        let shortOrgId = appCfg ^. #selfId
        getManager <-
          prepareAuthManager flowRt appEnv "Authorization" shortOrgId
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication manager: "
        authManager <- L.runIO getManager
        try (prepareRedisConnections $ appCfg ^. #redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
        migrateIfNeeded (appCfg ^. #migrationPath) (appCfg ^. #dbCfg) (appCfg ^. #autoMigrate)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (appCfg ^. #port))
        return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ run $ App.EnvR flowRt' appEnv
