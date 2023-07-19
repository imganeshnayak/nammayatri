{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOffer where

import Database.Beam.MySQL ()
import Domain.Types.DriverOffer
import Domain.Types.Estimate
import qualified EulerHS.Language as L
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO

createDriverOffer :: (L.MonadFlow m, Log m) => DriverOffer -> m ()
createDriverOffer driverOffer = do
  createWithKV driverOffer

findById :: (L.MonadFlow m, Log m) => Id DriverOffer -> m (Maybe DriverOffer)
findById (Id driverOfferId) = do
  findOneWithKV [Se.Is BeamDO.id $ Se.Eq driverOfferId]

findByBPPQuoteId :: (L.MonadFlow m, Log m) => Id BPPQuote -> m [DriverOffer]
findByBPPQuoteId (Id bppQuoteId) = findAllWithKV [Se.Is BeamDO.bppQuoteId $ Se.Eq bppQuoteId]

findByBPPQuoteIdInReplica :: (L.MonadFlow m, Log m) => Id BPPQuote -> m [DriverOffer]
findByBPPQuoteIdInReplica (Id bppQuoteId) = findAllWithKvInReplica [Se.Is BeamDO.bppQuoteId $ Se.Eq bppQuoteId]

-- updateStatus :: Id Estimate -> DriverOfferStatus -> SqlDB ()
-- updateStatus estimateId status = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverOfferStatus =. val status,
--         DriverOfferUpdatedAt =. val now
--       ]

updateStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Estimate -> DriverOfferStatus -> m ()
updateStatus (Id estimateId) status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDO.updatedAt now,
      Se.Set BeamDO.status status
    ]
    [Se.Is BeamDO.id (Se.Eq estimateId)]

instance FromTType' BeamDO.DriverOffer DriverOffer where
  fromTType' BeamDO.DriverOfferT {..} = do
    pure $
      Just
        DriverOffer
          { id = Id id,
            estimateId = Id estimateId,
            merchantId = Id <$> merchantId,
            driverName = driverName,
            durationToPickup = durationToPickup,
            distanceToPickup = distanceToPickup,
            validTill = validTill,
            bppQuoteId = Id bppQuoteId,
            rating = rating,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' BeamDO.DriverOffer DriverOffer where
  toTType' DriverOffer {..} = do
    BeamDO.DriverOfferT
      { BeamDO.id = getId id,
        BeamDO.estimateId = getId estimateId,
        BeamDO.merchantId = getId <$> merchantId,
        BeamDO.driverName = driverName,
        BeamDO.durationToPickup = durationToPickup,
        BeamDO.distanceToPickup = distanceToPickup,
        BeamDO.validTill = validTill,
        BeamDO.bppQuoteId = getId bppQuoteId,
        BeamDO.rating = rating,
        BeamDO.status = status,
        BeamDO.updatedAt = updatedAt
      }
