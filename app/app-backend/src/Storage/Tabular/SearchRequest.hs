{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMeters (..))
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as Domain
import qualified Storage.Tabular.Person as SP
import qualified Storage.Tabular.SearchReqLocation as SLoc

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      startTime UTCTime
      validTill UTCTime
      riderId SP.PersonTId
      fromLocationId SLoc.SearchReqLocationTId
      toLocationId SLoc.SearchReqLocationTId Maybe
      distance Double Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance TType SearchRequestT Domain.SearchRequest where
  fromTType SearchRequestT {..} = do
    return $
      Domain.SearchRequest
        { id = Id id,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey <$> toLocationId,
          distance = HighPrecMeters <$> distance,
          ..
        }
  toTType Domain.SearchRequest {..} =
    SearchRequestT
      { id = getId id,
        riderId = toKey riderId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> toLocationId,
        distance = getHighPrecMeters <$> distance,
        ..
      }
