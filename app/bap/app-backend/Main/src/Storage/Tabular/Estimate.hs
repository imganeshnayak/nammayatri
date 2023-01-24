{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Estimate where

import Beckn.External.Maps hiding (status)
import qualified Beckn.External.Maps as Domain
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import qualified Storage.Tabular.TripTerms as STripTerms

derivePersistField "Domain.LatLong"
derivePersistField "Domain.EstimateStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    EstimateT sql=estimate
      id Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      minTotalFare HighPrecMoney
      maxTotalFare HighPrecMoney
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      vehicleVariant VehVar.VehicleVariant
      driversLocation (PostgresList LatLong)
      tripTermsId STripTerms.TripTermsTId Maybe
      nightShiftMultiplier Centesimal Maybe
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      status Domain.EstimateStatus Maybe
      waitingChargePerMin Money Maybe
      waitingTimeEstimatedThreshold Seconds Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey EstimateT where
  type DomainKey EstimateT = Id Domain.Estimate
  fromKey (EstimateTKey _id) = Id _id
  toKey (Id id) = EstimateTKey id
