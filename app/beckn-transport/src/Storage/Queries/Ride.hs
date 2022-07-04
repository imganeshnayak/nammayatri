module Storage.Queries.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Domain.Types.RideBooking as Booking
import Domain.Types.Vehicle
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Person as Person
import Storage.Tabular.Ride as Ride
import Storage.Tabular.RideBooking as Booking
import Storage.Tabular.Vehicle as Vehicle
import Utils.Common

create :: Ride -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id Ride -> m (Maybe Ride)
findById = Esq.findById

findActiveByRBId :: Transactionable m => Id RideBooking -> m (Maybe Ride)
findActiveByRBId rbId =
  findOne $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val Ride.CANCELLED
    return ride

findAllCancelledByRBId :: Transactionable m => Id RideBooking -> m [Ride]
findAllCancelledByRBId rideBookingId =
  findAll $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rideBookingId)
        &&. ride ^. RideStatus ==. val Ride.CANCELLED
    return ride

findAllByVehicleId :: Transactionable m => Id Vehicle -> m [Ride]
findAllByVehicleId vehId =
  findAll $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideVehicleId ==. val (toKey vehId)
    return ride

findAllByDriverId ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  m [(Ride, RideBooking)]
findAllByDriverId driverId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbOnlyActive
  res <- Esq.findAll' $ do
    (ride :& rideBooking) <-
      from $
        table @RideT
          `innerJoin` table @RideBookingT
            `Esq.on` ( \(ride :& rideBooking) ->
                         ride ^. Ride.RideBookingId ==. rideBooking ^. Booking.RideBookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ ride ^. RideStatus `in_` valList [Ride.COMPLETED, Ride.CANCELLED])
    orderBy [desc $ ride ^. RideCreatedAt]
    limit limitVal
    offset offsetVal
    return (ride, rideBooking)
  fmap catMaybes $
    for res $ \(rideT :: RideT, bookingT) -> do
      fullBooking <- buildFullBooking bookingT
      return $ (extractSolidType rideT,) <$> fullBooking

findAllRideAPIEntityDataByRBId :: Transactionable m => Id RideBooking -> m [(Ride, Maybe Vehicle, Maybe Person)]
findAllRideAPIEntityDataByRBId rbId =
  Esq.findAll $ do
    (ride :& mbVehicle :& mbPerson) <-
      from $
        table @RideT
          `leftJoin` table @VehicleT
            `Esq.on` ( \(ride :& mbVehicle) ->
                         just (ride ^. Ride.RideVehicleId) ==. mbVehicle ?. Vehicle.VehicleTId
                     )
          `leftJoin` table @PersonT
            `Esq.on` ( \(ride :& _ :& mbPerson) ->
                         just (ride ^. Ride.RideDriverId) ==. (mbPerson ?. Person.PersonTId)
                     )
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return (ride, mbVehicle, mbPerson)

getInProgressByDriverId :: Transactionable m => Id Person -> m (Maybe Ride)
getInProgressByDriverId driverId =
  findOne $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    return ride

updateStatus ::
  Id Ride ->
  RideStatus ->
  SqlDB ()
updateStatus rideId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val status,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

updateStartTime ::
  Id Ride ->
  SqlDB ()
updateStartTime rideId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideTripStartTime =. val (Just now),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

updateStatusByIds ::
  [Id Ride] ->
  RideStatus ->
  SqlDB ()
updateStatusByIds ids status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val status,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId `in_` valList (toKey <$> ids)

updateDistance ::
  Id Person ->
  Double ->
  SqlDB ()
updateDistance driverId distance = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideTraveledDistance +=. val distance,
        RideUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RideDriverId ==. val (toKey driverId)
        &&. tbl ^. RideStatus ==. val Ride.INPROGRESS

updateAll ::
  Id Ride ->
  Ride ->
  SqlDB ()
updateAll rideId ride = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val ride.status,
        RideFare =. val ride.fare,
        RideTotalFare =. val ride.totalFare,
        RideChargeableDistance =. val (getHighPrecMeters <$> ride.chargeableDistance),
        RideTripEndTime =. val ride.tripEndTime,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

getCountByStatus :: Transactionable m => Id Organization -> m [(RideStatus, Int)]
getCountByStatus orgId = do
  Esq.findAll $ do
    (ride :& rideBooking) <-
      from $
        table @RideT
          `innerJoin` table @RideBookingT
            `Esq.on` ( \(ride :& rideBooking) ->
                         ride ^. Ride.RideBookingId ==. rideBooking ^. Booking.RideBookingTId
                     )
    where_ $ rideBooking ^. RideBookingProviderId ==. val (toKey orgId)
    groupBy $ ride ^. RideStatus
    return (ride ^. RideStatus, countRows :: SqlExpr (Esq.Value Int))
