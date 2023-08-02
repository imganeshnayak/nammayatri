{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), findOneWithKV, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceUsageConfig as BeamMSUC

-- findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
-- findByMerchantId merchId =
--   Esq.findOne $ do
--     merchantMapsCfg <- from $ table @MerchantServiceUsageConfigT
--     where_ $
--       merchantMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey merchId)
--     return merchantMapsCfg

findByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamMSUC.merchantId $ Se.Eq merchantId]

-- updateMerchantServiceUsageConfig ::
--   MerchantServiceUsageConfig ->
--   SqlDB ()
-- updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MerchantServiceUsageConfigGetDistances =. val getDistances,
--         MerchantServiceUsageConfigGetRoutes =. val getRoutes,
--         MerchantServiceUsageConfigSnapToRoad =. val snapToRoad,
--         MerchantServiceUsageConfigGetPlaceName =. val getPlaceName,
--         MerchantServiceUsageConfigGetPlaceDetails =. val getPlaceDetails,
--         MerchantServiceUsageConfigAutoComplete =. val autoComplete,
--         MerchantServiceUsageConfigSmsProvidersPriorityList =. val (PostgresList smsProvidersPriorityList),
--         MerchantServiceUsageConfigUpdatedAt =. val now
--       ]
--     where_ $
--       tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)

updateMerchantServiceUsageConfig :: (L.MonadFlow m, MonadTime m, Log m) => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamMSUC.getDistances getDistances,
      Se.Set BeamMSUC.getRoutes getRoutes,
      Se.Set BeamMSUC.snapToRoad snapToRoad,
      Se.Set BeamMSUC.getPlaceName getPlaceName,
      Se.Set BeamMSUC.getPlaceDetails getPlaceDetails,
      Se.Set BeamMSUC.autoComplete autoComplete,
      Se.Set BeamMSUC.smsProvidersPriorityList smsProvidersPriorityList,
      Se.Set BeamMSUC.updatedAt now
    ]
    [Se.Is BeamMSUC.merchantId (Se.Eq $ getId merchantId)]

instance FromTType' BeamMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  fromTType' BeamMSUC.MerchantServiceUsageConfigT {..} = do
    pure $
      Just
        MerchantServiceUsageConfig
          { merchantId = Id merchantId,
            initiateCall = initiateCall,
            notifyPerson = notifyPerson,
            getDistances = getDistances,
            getRoutes = getRoutes,
            snapToRoad = snapToRoad,
            getPlaceName = getPlaceName,
            getPickupRoutes = getPickupRoutes,
            getTripRoutes = getTripRoutes,
            getPlaceDetails = getPlaceDetails,
            autoComplete = autoComplete,
            getDistancesForCancelRide = getDistancesForCancelRide,
            smsProvidersPriorityList = smsProvidersPriorityList,
            whatsappProvidersPriorityList = whatsappProvidersPriorityList,
            useFraudDetection = useFraudDetection,
            enableDashboardSms = enableDashboardSms,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' BeamMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  toTType' MerchantServiceUsageConfig {..} = do
    BeamMSUC.MerchantServiceUsageConfigT
      { BeamMSUC.merchantId = getId merchantId,
        BeamMSUC.initiateCall = initiateCall,
        BeamMSUC.notifyPerson = notifyPerson,
        BeamMSUC.getDistances = getDistances,
        BeamMSUC.getRoutes = getRoutes,
        BeamMSUC.snapToRoad = snapToRoad,
        BeamMSUC.getPlaceName = getPlaceName,
        BeamMSUC.getPickupRoutes = getPickupRoutes,
        BeamMSUC.getTripRoutes = getTripRoutes,
        BeamMSUC.getPlaceDetails = getPlaceDetails,
        BeamMSUC.autoComplete = autoComplete,
        BeamMSUC.getDistancesForCancelRide = getDistancesForCancelRide,
        BeamMSUC.smsProvidersPriorityList = smsProvidersPriorityList,
        BeamMSUC.whatsappProvidersPriorityList = whatsappProvidersPriorityList,
        BeamMSUC.useFraudDetection = useFraudDetection,
        BeamMSUC.enableDashboardSms = enableDashboardSms,
        BeamMSUC.updatedAt = updatedAt,
        BeamMSUC.createdAt = createdAt
      }
