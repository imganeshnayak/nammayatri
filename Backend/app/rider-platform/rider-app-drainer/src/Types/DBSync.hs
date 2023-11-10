{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.DBSync
  ( module X,
    Env (..),
    Types.DBSync.Flow,
    History,
    StateRef (..),
    DBSyncConfig (..),
    DBCommand (..),
    CreateDBCommand (..),
    UpdateDBCommand (..),
    DeleteDBCommand (..),
  )
where

import Data.Aeson (Object)
import Database.Beam.Postgres (Connection)
import EulerHS.KVConnector.DBSync
import EulerHS.Language as EL
import EulerHS.Prelude
import Kafka.Producer as Producer
import Types.DBSync.Create as X
import Types.DBSync.Delete as X
import Types.DBSync.Update as X
import Types.Event as Event

data DBSyncConfig = DBSyncConfig
  { _emptyRetry :: Int,
    _rateLimitN :: Int,
    _rateLimitWindow :: Int,
    _streamReadCount :: Integer
  }

data Env = Env
  { _streamRedisInfo :: Text,
    _counterHandles :: Event.DBSyncCounterHandler,
    _kafkaConnection :: Producer.KafkaProducer,
    _pgConnection :: Connection,
    _dontEnableDbTables :: [Text],
    _dontEnableForKafka :: [Text]
  }

type Flow = EL.ReaderFlow Env

type History = [Int]

data StateRef = StateRef
  { _config :: DBSyncConfig,
    _history :: History
  }

data DBCommand
  = Create DBCommandVersion Tag Double DBName Object
  | Update DBCommandVersion Tag Double DBName Object
  | Delete DBCommandVersion Tag Double DBName Object
  deriving (Generic, FromJSON)

data CreateDBCommand = CreateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data UpdateDBCommand = UpdateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data DeleteDBCommand = DeleteDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

deriving stock instance Show EL.KVDBStreamEntryID

deriving stock instance Eq EL.KVDBStreamEntryID
