{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QFPDriverExtraFeeBounds
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QFPProgressiveDetPerExtraKmSlabs
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QFPSlabDetSlabs
import Storage.Queries.FullEntityBuilders (buildFullFarePolicy)
import Storage.Tabular.FarePolicy
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab ()
import Storage.Tabular.FarePolicy.Instances

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById farePolicyId = buildDType $ do
  res <- Esq.findById' (toKey farePolicyId)
  join <$> mapM buildFullFarePolicy res

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(FarePolicyT {..}, driverExtraFeeBoundsT, fpDetailsT) -> do
    Esq.update' $ \tbl -> do
      set
        tbl
        [ FarePolicyNightShiftStart =. val nightShiftStart,
          FarePolicyNightShiftEnd =. val nightShiftEnd,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

    updateDriverExtraFeeBounds driverExtraFeeBoundsT

    case fpDetailsT of
      ProgressiveDetailsT fpdd -> updateProgressiveDetails fpdd
      SlabsDetailsT fsdd -> updateSlabsDetails fsdd
  where
    updateDriverExtraFeeBounds driverExtraFeeBoundsT = do
      QFPDriverExtraFeeBounds.deleteAll' farePolicy.id
      Esq.createMany' driverExtraFeeBoundsT

    updateProgressiveDetails (FarePolicyProgressiveDetailsT {..}, perExtraKmRateSectionsT) = do
      Esq.update' $ \tbl -> do
        set
          tbl
          [ FarePolicyProgressiveDetailsBaseFare =. val baseFare,
            FarePolicyProgressiveDetailsBaseDistance =. val baseDistance,
            FarePolicyProgressiveDetailsDeadKmFare =. val deadKmFare,
            FarePolicyProgressiveDetailsNightShiftCharge =. val nightShiftCharge
          ]
        where_ $ tbl ^. FarePolicyProgressiveDetailsTId ==. val (toKey farePolicy.id)

      QFPProgressiveDetPerExtraKmSlabs.deleteAll' farePolicy.id
      Esq.createMany' perExtraKmRateSectionsT
    updateSlabsDetails dets = do
      QFPSlabDetSlabs.deleteAll' farePolicy.id
      Esq.createMany' dets
