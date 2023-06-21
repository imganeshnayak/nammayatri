{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Common
import qualified Storage.Beam.Merchant as BeamM
import Storage.Tabular.Merchant

findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantShortId ==. val (getShortId shortId_)
    return merchant

findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantSubscriberId ==. val (getShortId subscriberId)
    return merchant

findAll :: Transactionable m => m [Merchant]
findAll =
  Esq.findAll $ do from $ table @MerchantT

update :: Merchant -> SqlDB ()
update merchant = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantName =. val merchant.name,
        MerchantGatewayUrl =. val (showBaseUrl merchant.gatewayUrl),
        MerchantRegistryUrl =. val (showBaseUrl merchant.registryUrl),
        MerchantUpdatedAt =. val now
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey merchant.id)

transformBeamMerchantToDomain :: L.MonadFlow m => BeamM.Merchant -> m Merchant
transformBeamMerchantToDomain BeamM.MerchantT {..} = do
  gwUrl <- parseBaseUrl gatewayUrl
  regUrl <- parseBaseUrl registryUrl
  doBaseUrl <- parseBaseUrl driverOfferBaseUrl
  let geofencingConfig =
        Geo.GeofencingConfig
          { origin = originRestriction,
            destination = destinationRestriction
          }
  pure $
    Merchant
      { id = Id id,
        subscriberId = ShortId subscriberId,
        shortId = ShortId shortId,
        name = name,
        city = city,
        geofencingConfig = geofencingConfig,
        gatewayUrl = gwUrl,
        registryUrl = regUrl,
        driverOfferBaseUrl = doBaseUrl,
        driverOfferApiKey = driverOfferApiKey,
        driverOfferMerchantId = driverOfferMerchantId,
        geoHashPrecisionValue = geoHashPrecisionValue,
        signingPublicKey = signingPublicKey,
        cipherText = cipherText,
        signatureExpiry = signatureExpiry,
        createdAt = createdAt,
        updatedAt = updatedAt
      }

transformDomainMerchantToBeam :: Merchant -> BeamM.Merchant
transformDomainMerchantToBeam Merchant {..} = do
  let Geo.GeofencingConfig {..} = geofencingConfig
  BeamM.defaultMerchant
    { BeamM.id = getId id,
      BeamM.subscriberId = getShortId subscriberId,
      BeamM.shortId = getShortId shortId,
      BeamM.name = name,
      BeamM.city = city,
      BeamM.originRestriction = origin,
      BeamM.destinationRestriction = destination,
      BeamM.gatewayUrl = showBaseUrl gatewayUrl,
      BeamM.registryUrl = showBaseUrl registryUrl,
      BeamM.driverOfferBaseUrl = showBaseUrl driverOfferBaseUrl,
      BeamM.driverOfferApiKey = driverOfferApiKey,
      BeamM.driverOfferMerchantId = driverOfferMerchantId,
      BeamM.geoHashPrecisionValue = geoHashPrecisionValue,
      BeamM.signingPublicKey = signingPublicKey,
      BeamM.cipherText = cipherText,
      BeamM.signatureExpiry = signatureExpiry,
      BeamM.createdAt = createdAt,
      BeamM.updatedAt = updatedAt
    }
