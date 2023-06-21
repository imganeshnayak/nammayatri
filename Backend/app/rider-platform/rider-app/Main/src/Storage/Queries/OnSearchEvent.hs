{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.OnSearchEvent where

import Domain.Types.OnSearchEvent
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.OnSearchEvent as BeamOSE
import Storage.Tabular.OnSearchEvent ()

create :: OnSearchEvent -> SqlDB ()
create = Esq.create

transformBeamOnSearchEventToDomain :: BeamOSE.OnSearchEvent -> OnSearchEvent
transformBeamOnSearchEventToDomain BeamOSE.OnSearchEventT {..} = do
  OnSearchEvent
    { id = Id id,
      bppId = bppId,
      messageId = messageId,
      errorCode = errorCode,
      errorType = errorType,
      errorMessage = errorMessage,
      createdAt = createdAt
    }

transformDomainOnSearchEventToBeam :: OnSearchEvent -> BeamOSE.OnSearchEvent
transformDomainOnSearchEventToBeam OnSearchEvent {..} =
  BeamOSE.defaultOnSearchEvent
    { BeamOSE.id = getId id,
      BeamOSE.bppId = bppId,
      BeamOSE.messageId = messageId,
      BeamOSE.errorCode = errorCode,
      BeamOSE.errorType = errorType,
      BeamOSE.errorMessage = errorMessage,
      BeamOSE.createdAt = createdAt
    }
