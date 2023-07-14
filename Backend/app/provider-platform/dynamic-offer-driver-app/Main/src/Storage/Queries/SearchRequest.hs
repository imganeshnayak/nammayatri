{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequest where

import Domain.Types.SearchRequest as Domain
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as BeamSR
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createDSReq :: (L.MonadFlow m, Log m) => SearchRequest -> m ()
createDSReq = createWithKV

create :: (L.MonadFlow m, Log m) => SearchRequest -> m ()
create dsReq = createDSReq dsReq >> QSRL.create dsReq.fromLocation >> QSRL.create dsReq.toLocation

findById :: (L.MonadFlow m, Log m) => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

getRequestIdfromTransactionId :: (L.MonadFlow m, Log m) => Id SearchRequest -> m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId (Id tId) = findOneWithKV [Se.Is BeamSR.transactionId $ Se.Eq tId] <&> fmap Domain.id

findByTransactionId :: (L.MonadFlow m, Log m) => Text -> m (Maybe (Id SearchRequest))
findByTransactionId transactionId = findOneWithKV [Se.And [Se.Is BeamSR.transactionId $ Se.Eq transactionId]] <&> (Domain.id <$>)

updateAutoAssign ::
  (L.MonadFlow m, Log m) =>
  Id SearchRequest ->
  Bool ->
  m ()
updateAutoAssign searchRequestId autoAssignedEnabled =
  updateWithKV
    [Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled]
    [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]

instance FromTType' BeamSR.SearchRequest SearchRequest where
  fromTType' BeamSR.SearchRequestT {..} = do
    fl <- QSRL.findById (Id fromLocationId)
    tl <- QSRL.findById (Id toLocationId)
    pUrl <- parseBaseUrl bapUri
    if isJust fl && isJust tl
      then
        pure $
          Just
            SearchRequest
              { id = Id id,
                transactionId = transactionId,
                providerId = Id providerId,
                fromLocation = fromJust fl,
                toLocation = fromJust tl,
                area = area,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                estimatedDistance = estimatedDistance,
                estimatedDuration = estimatedDuration,
                customerLanguage = customerLanguage,
                device = device,
                createdAt = createdAt,
                specialLocationTag = specialLocationTag,
                autoAssignEnabled = autoAssignEnabled
              }
      else pure Nothing

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} = do
    BeamSR.SearchRequestT
      { BeamSR.id = getId id,
        BeamSR.transactionId = transactionId,
        BeamSR.providerId = getId providerId,
        BeamSR.fromLocationId = getId fromLocation.id,
        BeamSR.toLocationId = getId toLocation.id,
        BeamSR.area = area,
        BeamSR.bapId = bapId,
        BeamSR.bapUri = showBaseUrl bapUri,
        BeamSR.bapCity = bapCity,
        BeamSR.bapCountry = bapCountry,
        BeamSR.estimatedDistance = estimatedDistance,
        BeamSR.estimatedDuration = estimatedDuration,
        BeamSR.customerLanguage = customerLanguage,
        BeamSR.device = device,
        BeamSR.createdAt = createdAt,
        BeamSR.autoAssignEnabled = autoAssignEnabled,
        BeamSR.specialLocationTag = specialLocationTag
      }

-- updateAutoAssign ::
--   Id SearchRequest ->
--   Bool ->
--   SqlDB ()
-- updateAutoAssign searchRequestId autoAssignedEnabled = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ SearchRequestAutoAssignEnabled =. val (Just autoAssignedEnabled)
--       ]
