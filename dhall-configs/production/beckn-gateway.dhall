let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_gateway"
  }

let pgcfg =
  { connTag = "gatewayDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_gateway"
  }

let rcfg =
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

let coreVersions =
  { mobility = "0.8.2"
  , finalMileDelivery = "0.9.1"
  , localRetail = "0.9.1"
  , foodAndBeverage = "0.9.1"
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = "JUSPAY.BG.1"
, hostName = "juspay.in"
, nwAddress = "https://api.beckn.juspay.in/gateway/v1/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, searchTimeout = None Integer
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, coreVersions = coreVersions
, mobilityDomainVersion = "0.8.2"
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, httpClientOptions = httpClientOptions

}
