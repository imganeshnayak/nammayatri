let sec = ./secrets/common.dhall

let defaultPoolConfig =
    { stripes = +1
    , keepAlive = +10
    , resourcesPerStripe = +50
    }

let smsSessionConfig =
  { attempts = +3
  , authExpiry = +3
  , tokenExpiry = +365
  }

{-
let exotelCfg : ExotelCfg =
  { apiKey = ""
  , apiToken = ""
  , sid = ""
  , callerId = ""
  }
-}

let TraceFlag = < TRACE_INCOMING | TRACE_OUTGOING | TRACE_ALL | TRACE_NOTHING >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let LoggerConfig = 
  { level : LogLevel
  , isAsync : Bool
  , logToFile : Bool
  , logFilePath : Text
  , logToConsole : Bool
  , logRawSql : Bool
  }  

in { defaultPoolConfig = defaultPoolConfig
   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , smsSessionConfig = smsSessionConfig
   -- , exotelCfg
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = None Text
   , autoMigrate = False
   , TraceFlag = TraceFlag
   , LogLevel = LogLevel
   , LoggerConfig = LoggerConfig
   }
