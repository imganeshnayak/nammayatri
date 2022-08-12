{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Common (HighPrecMeters (HighPrecMeters))
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      organizationId OrganizationTId
      vehicleVariant Variant.Variant

      baseDistancePerKmFare Amount
      baseDistanceMeters Double
      extraKmFare Amount
      deadKmFare Amount
      driverExtraFeeList (PostgresList Double)

      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Double Maybe
      createdAt UTCTime
      updatedAt UTCTime
      UniqueFarePolicyId id
      Primary id
      deriving Generic
    |]

instance TEntityKey FarePolicyT where
  type DomainKey FarePolicyT = Id Domain.FarePolicy
  fromKey (FarePolicyTKey _id) = Id _id
  toKey (Id id) = FarePolicyTKey id

instance TType FarePolicyT Domain.FarePolicy where
  fromTType FarePolicyT {..} = do
    return $
      Domain.FarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseDistanceMeters = HighPrecMeters baseDistanceMeters,
          nightShiftRate = nightShiftRate,
          driverExtraFeeList = map roundToIntegral $ unPostgresList driverExtraFeeList,
          deadKmFare = roundToIntegral deadKmFare,
          ..
        }

  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        baseDistanceMeters = baseDistanceMeters.getHighPrecMeters,
        driverExtraFeeList = PostgresList $ map fromIntegral driverExtraFeeList,
        deadKmFare = fromIntegral deadKmFare,
        ..
      }
