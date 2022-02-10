module Environment where

import Beckn.Prelude
import Beckn.Storage.Hedis (HedisCfg, HedisEnv, connectHedis)
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import Tools.Kafka
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    kafkaConsumerCfgs :: KafkaConsumerCfgs,
    hedisCfg :: HedisCfg
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    kafkaConsumerEnv :: KafkaConsumerEnv,
    hedisEnv :: HedisEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  kafkaConsumerEnv <- buildKafkaConsumerEnv kafkaConsumerCfgs
  hedisEnv <- connectHedis hedisCfg
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  releaseKafkaConsumerEnv kafkaConsumerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
