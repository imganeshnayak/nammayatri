module Storage.Queries.Rating where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.Rating
import Domain.Types.Ride
import Storage.Tabular.Rating
import Utils.Common

create :: Rating -> SqlDB ()
create = Esq.create'

updateRatingValue :: Id Rating -> Id Person -> Int -> SqlDB ()
updateRatingValue ratingId driverId newRatingValue = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RatingRatingValue =. val newRatingValue,
        RatingUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RatingTId ==. val (toKey ratingId)
        &&. tbl ^. RatingDriverId ==. val (toKey driverId)

findByRideId :: Transactionable m => Id Ride -> m (Maybe Rating)
findByRideId rideId =
  findOne $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingRideId ==. val (toKey rideId)
    return rating

findAllRatingsForPerson :: Transactionable m => Id Person -> m [Rating]
findAllRatingsForPerson driverId =
  findAll $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating
