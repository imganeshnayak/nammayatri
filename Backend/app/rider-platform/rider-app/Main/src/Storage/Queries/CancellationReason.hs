{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason
import qualified Domain.Types.CancellationReason as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Storage.Beam.CancellationReason as BeamCR
import Storage.Tabular.CancellationReason

findAll :: Transactionable m => CancellationStage -> m [CancellationReason]
findAll cancStage =
  Esq.findAll $ do
    cancellationReason <- from $ table @CancellationReasonT
    where_ $
      cancellationReason ^. CancellationReasonEnabled
        &&. case cancStage of
          OnSearch -> cancellationReason ^. CancellationReasonOnSearch
          OnConfirm -> cancellationReason ^. CancellationReasonOnConfirm
          OnAssign -> cancellationReason ^. CancellationReasonOnAssign
    orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
    return cancellationReason

transformBeamCancellationReasonToDomain :: BeamCR.CancellationReason -> CancellationReason
transformBeamCancellationReasonToDomain BeamCR.CancellationReasonT {..} = do
  CancellationReason
    { reasonCode = Domain.CancellationReasonCode reasonCode,
      description = description,
      enabled = enabled,
      onSearch = onSearch,
      onConfirm = onConfirm,
      onAssign = onAssign,
      priority = priority
    }

transformDomainCancellationReasonToBeam :: CancellationReason -> BeamCR.CancellationReason
transformDomainCancellationReasonToBeam CancellationReason {..} =
  BeamCR.defaultCancellationReason
    { BeamCR.reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
      BeamCR.description = description,
      BeamCR.enabled = enabled,
      BeamCR.onSearch = onSearch,
      BeamCR.onConfirm = onConfirm,
      BeamCR.onAssign = onAssign,
      BeamCR.priority = priority
    }
