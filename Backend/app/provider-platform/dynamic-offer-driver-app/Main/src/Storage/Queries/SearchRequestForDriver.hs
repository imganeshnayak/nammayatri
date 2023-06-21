{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequestForDriver where

import qualified Data.Time as T
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD

createMany :: L.MonadFlow m => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: L.MonadFlow m => SearchRequestForDriver -> m ()
    createOne searchRequestForDriver = do
      dbConf <- L.getOption KBT.PsqlDbCfg
      let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
      let updatedMeshConfig = setMeshConfig modelName
      case dbConf of
        Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSearchRequestForDriverToBeam searchRequestForDriver)
        Nothing -> pure ()

findAllActiveBySTId :: L.MonadFlow m => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveBySTId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamSRFD.id $ Se.Eq searchTryId,
                Se.Is BeamSRFD.status $ Se.Eq Domain.Active
              ]
          ]
    Nothing -> pure []

findAllActiveBySRId :: L.MonadFlow m => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId (Id searchReqId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamSRFD.requestId $ Se.Eq searchReqId,
                Se.Is BeamSRFD.status $ Se.Eq Domain.Active
              ]
          ]
    Nothing -> pure []

findAllActiveWithoutRespBySearchTryId :: (L.MonadFlow m, MonadTime m) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchTryId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
                  <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
                  <> [Se.Is BeamSRFD.response $ Se.Eq Nothing]
              )
          ]
    Nothing -> pure []

findByDriverAndSearchTryId :: L.MonadFlow m => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure Nothing) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
                  <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
                  <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
              )
          ]
    Nothing -> pure Nothing

findByDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
          [Se.And [Se.Is BeamSRFD.driverId $ Se.Eq driverId, Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]]
    Nothing -> pure []

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamSRFD.driverId (Se.Eq personId)]
    Nothing -> pure ()

setInactiveBySTId :: L.MonadFlow m => Id SearchTry -> m (MeshResult ())
setInactiveBySTId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [Se.Set BeamSRFD.status Domain.Inactive]
        [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

setInactiveBySRId :: L.MonadFlow m => Id SearchRequest -> m (MeshResult ())
setInactiveBySRId (Id searchReqId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [Se.Set BeamSRFD.status Domain.Inactive]
        [Se.Is BeamSRFD.requestId (Se.Eq searchReqId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateDriverResponse :: L.MonadFlow m => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> m (MeshResult ())
updateDriverResponse (Id id) response = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRFD.SearchRequestForDriverT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [Se.Set BeamSRFD.response (Just response)]
        [Se.Is BeamSRFD.id (Se.Eq id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamSearchRequestForDriverToDomain :: BeamSRFD.SearchRequestForDriver -> SearchRequestForDriver
transformBeamSearchRequestForDriverToDomain BeamSRFD.SearchRequestForDriverT {..} = do
  SearchRequestForDriver
    { id = Id id,
      requestId = Id requestId,
      searchTryId = Id searchTryId,
      merchantId = Id <$> merchantId,
      startTime = startTime,
      searchRequestValidTill = T.localTimeToUTC T.utc searchRequestValidTill,
      driverId = Id driverId,
      actualDistanceToPickup = actualDistanceToPickup,
      straightLineDistanceToPickup = straightLineDistanceToPickup,
      durationToPickup = durationToPickup,
      vehicleVariant = vehicleVariant,
      status = status,
      batchNumber = batchNumber,
      lat = lat,
      lon = lon,
      createdAt = T.localTimeToUTC T.utc createdAt,
      response = response,
      driverMinExtraFee = driverMinExtraFee,
      driverMaxExtraFee = driverMaxExtraFee,
      rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
      isPartOfIntelligentPool = isPartOfIntelligentPool,
      cancellationRatio = cancellationRatio,
      acceptanceRatio = acceptanceRatio,
      driverAvailableTime = driverAvailableTime,
      parallelSearchRequestCount = parallelSearchRequestCount,
      keepHiddenForSeconds = keepHiddenForSeconds,
      driverSpeed = driverSpeed,
      mode = mode
    }

transformDomainSearchRequestForDriverToBeam :: SearchRequestForDriver -> BeamSRFD.SearchRequestForDriver
transformDomainSearchRequestForDriverToBeam SearchRequestForDriver {..} =
  BeamSRFD.SearchRequestForDriverT
    { BeamSRFD.id = getId id,
      BeamSRFD.requestId = getId requestId,
      BeamSRFD.searchTryId = getId searchTryId,
      BeamSRFD.merchantId = getId <$> merchantId,
      BeamSRFD.startTime = startTime,
      BeamSRFD.searchRequestValidTill = T.utcToLocalTime T.utc searchRequestValidTill,
      BeamSRFD.driverId = getId driverId,
      BeamSRFD.actualDistanceToPickup = actualDistanceToPickup,
      BeamSRFD.straightLineDistanceToPickup = straightLineDistanceToPickup,
      BeamSRFD.durationToPickup = durationToPickup,
      BeamSRFD.vehicleVariant = vehicleVariant,
      BeamSRFD.status = status,
      BeamSRFD.batchNumber = batchNumber,
      BeamSRFD.lat = lat,
      BeamSRFD.lon = lon,
      BeamSRFD.createdAt = T.utcToLocalTime T.utc createdAt,
      BeamSRFD.response = response,
      BeamSRFD.driverMinExtraFee = driverMinExtraFee,
      BeamSRFD.driverMaxExtraFee = driverMaxExtraFee,
      BeamSRFD.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
      BeamSRFD.isPartOfIntelligentPool = isPartOfIntelligentPool,
      BeamSRFD.cancellationRatio = cancellationRatio,
      BeamSRFD.acceptanceRatio = acceptanceRatio,
      BeamSRFD.driverAvailableTime = driverAvailableTime,
      BeamSRFD.parallelSearchRequestCount = parallelSearchRequestCount,
      BeamSRFD.keepHiddenForSeconds = keepHiddenForSeconds,
      BeamSRFD.driverSpeed = driverSpeed,
      BeamSRFD.mode = mode
    }
