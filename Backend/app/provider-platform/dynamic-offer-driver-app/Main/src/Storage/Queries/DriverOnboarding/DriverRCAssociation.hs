{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import Domain.Types.DriverOnboarding.DriverRCAssociation as DRCA
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRCT
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as QVRC

-- create :: DriverRCAssociation -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DriverRCAssociation -> m ()
create driverRCAssociation = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverRCAssociationToBeam driverRCAssociation)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id DriverRCAssociation -> m (Maybe DriverRCAssociation)
findById (Id drcaId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDRCA.id $ Se.Eq drcaId]
    Nothing -> pure Nothing

getActiveAssociationByDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m (Maybe DriverRCAssociation)
getActiveAssociationByDriver (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq personId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]
    Nothing -> pure Nothing

-- findAllByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m [(DriverRCAssociation, VehicleRegistrationCertificate)]
-- findAllByDriverId driverId = do
--   findAll $ do
--     rcAssoc :& regCert <-
--       from $
--         table @DriverRCAssociationT
--           `Esq.innerJoin` table @VehicleRegistrationCertificateT
--             `Esq.on` ( \(rcAssoc :& regCert) ->
--                          rcAssoc ^. DriverRCAssociationRcId ==. regCert ^. VehicleRegistrationCertificateTId
--                      )
--     where_ $
--       rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--     orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedOn]
--     return (rcAssoc, regCert)

findAllvehicleRCFromDriverRCAssociation :: L.MonadFlow m => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
findAllvehicleRCFromDriverRCAssociation driverRCAssociation = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamVRCT.VehicleRegistrationCertificateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (QVRC.transformBeamVehicleRegistrationCertificateToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamVRCT.id $ Se.In $ getId . DRCA.rcId <$> driverRCAssociation]
    Nothing -> pure []

findAllByDriverId :: L.MonadFlow m => Id Person -> m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      driverRCA <- either (pure []) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDRCA.driverId $ Se.Eq driverId] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing
      vehicleRC <- findAllvehicleRCFromDriverRCAssociation driverRCA
      let rcAWithrc = foldl' (getRCAWithRC vehicleRC) [] driverRCA
      pure rcAWithrc
    Nothing -> pure []
  where
    getRCAWithRC vrc acc driverRCA' =
      let vrc' = filter (\v -> v.id == driverRCA'.rcId) vrc
       in acc <> ((\v -> (driverRCA', v)) <$> vrc')

-- getActiveAssociationByRC ::
--   (Transactionable m, MonadFlow m) =>
--   Id VehicleRegistrationCertificate ->
--   m (Maybe DriverRCAssociation)
-- getActiveAssociationByRC rcId = do
--   now <- getCurrentTime
--   findOne $ do
--     association <- from $ table @DriverRCAssociationT
--     where_ $
--       association ^. DriverRCAssociationRcId ==. val (toKey rcId)
--         &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
--     return association

getActiveAssociationByRC :: (L.MonadFlow m, MonadTime m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
getActiveAssociationByRC (Id rcId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]
    Nothing -> pure Nothing

endAssociation :: (L.MonadFlow m, MonadTime m) => Id Person -> m ()
endAssociation (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamDRCA.associatedTill $ Just now
          ]
          [Se.And [Se.Is BeamDRCA.id (Se.Eq driverId), Se.Is BeamDRCA.associatedTill (Se.GreaterThan $ Just now)]]
    Nothing -> pure ()

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamDRCA.driverId (Se.Eq driverId)]
    Nothing -> pure ()

transformBeamDriverRCAssociationToDomain :: BeamDRCA.DriverRCAssociation -> DriverRCAssociation
transformBeamDriverRCAssociationToDomain BeamDRCA.DriverRCAssociationT {..} = do
  DriverRCAssociation
    { id = Id id,
      driverId = Id driverId,
      rcId = Id rcId,
      associatedOn = associatedOn,
      associatedTill = associatedTill,
      consent = consent,
      consentTimestamp = consentTimestamp
    }

transformDomainDriverRCAssociationToBeam :: DriverRCAssociation -> BeamDRCA.DriverRCAssociation
transformDomainDriverRCAssociationToBeam DriverRCAssociation {..} =
  BeamDRCA.DriverRCAssociationT
    { BeamDRCA.id = getId id,
      BeamDRCA.driverId = getId driverId,
      BeamDRCA.rcId = getId rcId,
      BeamDRCA.associatedOn = associatedOn,
      BeamDRCA.associatedTill = associatedTill,
      BeamDRCA.consent = consent,
      BeamDRCA.consentTimestamp = consentTimestamp
    }
