module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
