{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

-- import Config.Config as Config
import Config.Env as Env
import qualified Constants as C
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
import qualified Data.HashSet as HS
import qualified "unordered-containers" Data.HashSet as HashSet
import qualified Data.Text as T
import Environment
import qualified Euler.Events.Network as NW
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Interpreters as R
import EulerHS.Logger.Types
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Event.Event as Event
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Types.DBSync
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "dynamic-offer-driver-app"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig

  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime
      (Just loggerRt)
      ( \flowRt -> do
          putStrLn @String "Initializing DB and KV Connections..."
          runFlow
            flowRt
            ( prepareConnectionDriver
                ConnectionConfigDriver
                  { esqDBCfg = appCfg.esqDBCfg,
                    esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                    hedisClusterCfg = appCfg.hedisClusterCfg,
                    locationDbCfg = appCfg.esqLocationDBCfg,
                    locationDbReplicaCfg = appCfg.esqLocationDBRepCfg
                  }
                appCfg.tables
            )
          dbSyncMetric <- Event.mkDBSyncMetric
          let environment = Env (T.pack C.kvRedis) dbSyncMetric
          R.runFlow flowRt (runReaderT DBSync.startDBSync environment)
      )