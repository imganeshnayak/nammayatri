{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SavedReqLocation where

import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SavedReqLocation

create :: SavedReqLocation -> SqlDB ()
create = Esq.create

findByLatLonAndRiderId :: Transactionable m => Id Person -> LatLong -> m (Maybe SavedReqLocation)
findByLatLonAndRiderId personId LatLong {..} = Esq.findOne $ do
  saveReqLocation <- from $ table @SavedReqLocationT
  where_ $
    saveReqLocation ^. SavedReqLocationLat ==. val lat
      &&. saveReqLocation ^. SavedReqLocationLon ==. val lon
      &&. saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey personId)
  return saveReqLocation

findAllByRiderId :: Transactionable m => Id Person -> m [SavedReqLocation]
findAllByRiderId perId =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
    orderBy [desc $ saveReqLocation ^. SavedReqLocationUpdatedAt]
    return saveReqLocation

deleteByRiderIdAndTag :: Id Person -> Text -> SqlDB ()
deleteByRiderIdAndTag perId addressTag = do
  Esq.delete $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)

findAllByRiderIdAndTag :: Transactionable m => Id Person -> Text -> m [SavedReqLocation]
findAllByRiderIdAndTag perId addressTag =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)
    return saveReqLocation

deleteAllByRiderId :: Id Person -> SqlDB ()
deleteAllByRiderId personId = do
  Esq.delete $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey personId))
