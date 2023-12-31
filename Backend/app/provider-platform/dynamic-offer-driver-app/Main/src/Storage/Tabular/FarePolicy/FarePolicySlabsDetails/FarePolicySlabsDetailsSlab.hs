{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Minutes, Money)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.Table (FarePolicyTId)

derivePersistFieldJSON "Domain.PlatformFeeCharge"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicySlabsDetailsSlabT sql=fare_policy_slabs_details_slab
      Id Int
      farePolicyId FarePolicyTId
      startDistance Meters
      baseFare Money
      platformFeeCharge Domain.PlatformFeeCharge Maybe
      platformFeeCgst Double Maybe
      platformFeeSgst Double Maybe
      waitingCharge Domain.WaitingCharge Maybe
      freeWatingTime Minutes Maybe
      nightShiftCharge Domain.NightShiftCharge Maybe

      deriving Generic
    |]

type FullFarePolicySlabsDetailsSlab = (Id Domain.FarePolicy, Domain.FPSlabsDetailsSlab)

instance FromTType FarePolicySlabsDetailsSlabT FullFarePolicySlabsDetailsSlab where
  fromTType FarePolicySlabsDetailsSlabT {..} = do
    let waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              }
        platformFeeInfo =
          ((,,) <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst) <&> \(platformFeeCharge', platformFeeCgst', platformFeeSgst') ->
            Domain.PlatformFeeInfo
              { platformFeeCharge = platformFeeCharge',
                cgst = platformFeeCgst',
                sgst = platformFeeSgst'
              }
    return
      ( fromKey farePolicyId,
        Domain.FPSlabsDetailsSlab
          { ..
          }
      )

instance ToTType FarePolicySlabsDetailsSlabT FullFarePolicySlabsDetailsSlab where
  toTType (farePolicyId, Domain.FPSlabsDetailsSlab {..}) = do
    FarePolicySlabsDetailsSlabT
      { farePolicyId = toKey farePolicyId,
        waitingCharge = waitingChargeInfo <&> (.waitingCharge),
        freeWatingTime = waitingChargeInfo <&> (.freeWaitingTime),
        platformFeeCharge = platformFeeInfo <&> (.platformFeeCharge),
        platformFeeCgst = platformFeeInfo <&> (.cgst),
        platformFeeSgst = platformFeeInfo <&> (.sgst),
        ..
      }
