let common = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let transporter = ./dynamic-offer-driver-app.dhall

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = True
              , logFilePath = "/tmp/driver-offer-scheduler.log"
              , prettyPrinting = True
              }
      , esqDBCfg = appCfg.esqDBCfg
      , metricsPort = +8056
      , hedisCfg = appCfg.hedisCfg
      , hedisClusterCfg = appCfg.hedisClusterCfg
      , hedisNonCriticalCfg = appCfg.hedisCfg
      , hedisNonCriticalClusterCfg = appCfg.hedisClusterCfg
      , hedisMigrationStage = True
      , cutOffHedisCluster = False
      , hedisPrefix = "driver-offer-scheduler"
      , port = +8055
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      , enableRedisLatencyLogging = False
      , enablePrometheusMetricLogging = True
      , groupName = appCfg.groupName
      , schedulerType = common.schedulerType.DbBased
      , schedulerSetName = appCfg.schedulerSetName
      , streamName = appCfg.streamName
      , maxThreads = +10
      , block = +10000
      , readCount = +1
      }

in  { appCfg =
            appCfg
        //  { loggerConfig = appCfg.loggerConfig
            , groupName = appCfg.groupName
            , schedulerType = appCfg.schedulerType
            , schedulerSetName = appCfg.schedulerSetName
            , streamName =
                    appCfg.streamName
                //  { logFilePath = "/tmp/driver-offer-allocator.log" }
            }
    , schedulerConfig
    }
