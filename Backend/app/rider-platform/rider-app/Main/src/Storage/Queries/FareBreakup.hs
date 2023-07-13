{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FareBreakup where

import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FareBreakup as BeamFB

-- createMany :: [FareBreakup] -> SqlDB ()
-- createMany = Esq.createMany

create :: L.MonadFlow m => FareBreakup -> m ()
create fareBreakup = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFB.FareBreakupT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainFareBreakupToBeam fareBreakup)
    Nothing -> pure ()

createMany :: L.MonadFlow m => [FareBreakup] -> m ()
createMany = traverse_ create

-- findAllByBookingId :: (MonadThrow m, Log m, Transactionable m) => Id Booking -> m [FareBreakup]
-- findAllByBookingId bookingId =
--   findAll $ do
--     fareBreakup <- from $ table @FareBreakupT
--     where_ $ fareBreakup ^. FareBreakupBookingId ==. val (toKey bookingId)
--     return fareBreakup

findAllByBookingId :: L.MonadFlow m => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFB.FareBreakupT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamFareBreakupToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId]
    Nothing -> pure []

transformBeamFareBreakupToDomain :: BeamFB.FareBreakup -> FareBreakup
transformBeamFareBreakupToDomain BeamFB.FareBreakupT {..} = do
  FareBreakup
    { id = Id id,
      bookingId = Id bookingId,
      description = description,
      amount = amount
    }

transformDomainFareBreakupToBeam :: FareBreakup -> BeamFB.FareBreakup
transformDomainFareBreakupToBeam FareBreakup {..} =
  BeamFB.FareBreakupT
    { BeamFB.id = getId id,
      BeamFB.bookingId = getId bookingId,
      BeamFB.description = description,
      BeamFB.amount = amount
    }
