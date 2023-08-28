{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Person
import qualified EulerHS.Language as L
-- import Kernel.Storage.Esqueleto as Esq

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Kernel.Types.Time
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF

-- create :: DriverFee -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DriverFee -> m ()
create = createWithKV

-- findById :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

-- findByShortId :: Transactionable m => ShortId DriverFee -> m (Maybe DriverFee)
-- findByShortId shortId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $ driverFee ^. DriverFeeShortId ==. val (getShortId shortId)
--     return driverFee

-- findByShortId :: (L.MonadFlow m, Log m) => ShortId DriverFee -> m (Maybe DriverFee)
-- findByShortId shortId = findOneWithKV [Se.Is BeamDF.shortId $ Se.Eq $ getShortId shortId]

-- findPendingFeesByDriverFeeId :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findPendingFeesByDriverFeeId driverFeeId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeId ==. val (getId driverFeeId)
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverFeeId :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.Eq driverFeeId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]
        ]
    ]

-- findPendingFeesByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findPendingFeesByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey (cast driverId))
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m [DriverFee]
findPendingFeesByDriverId (Id driverId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

-- findLatestFeeByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findLatestFeeByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
--     orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
--     limit 1
--     return driverFee

findLatestFeeByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [Se.Is BeamDF.driverId $ Se.Eq driverId]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestRegisterationFeeByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findLatestRegisterationFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- pure $ case res of
--   (x : _) -> Just x
--   _ -> Nothing

-- findOldestFeeByStatus :: Transactionable m => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
-- findOldestFeeByStatus driverId status = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
--         &&. driverFee ^. DriverFeeStatus ==. val status
--     orderBy [asc $ driverFee ^. DriverFeeCreatedAt]
--     limit 1
--     return driverFee

findOldestFeeByStatus :: (L.MonadFlow m, Log m) => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus (Id driverId) status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq status
        ]
    ]
    (Se.Asc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- findFeesInRangeWithStatus :: Transactionable m => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
-- findFeesInRangeWithStatus startTime endTime status = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeStartTime >=. val startTime
--         &&. driverFee ^. DriverFeeEndTime <=. val endTime
--         &&. driverFee ^. DriverFeeStatus ==. val status
--     return driverFee

findFeesInRangeWithStatus :: (L.MonadFlow m, Log m) => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
findFeesInRangeWithStatus startTime endTime status =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Or [Se.Is BeamDF.status (Se.Eq ONGOING), Se.Is BeamDF.payBy (Se.LessThanOrEq endTime)],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

-- findWindowsWithStatus :: Transactionable m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
-- findWindowsWithStatus driverId startTime endTime mbStatus limitVal offsetVal = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--         &&. driverFee ^. DriverFeeStartTime >=. val startTime
--         &&. driverFee ^. DriverFeeEndTime <=. val endTime
--         &&. whenJust_ mbStatus (\status -> driverFee ^. DriverFeeStatus ==. val status)
--     limit $ fromIntegral limitVal
--     offset $ fromIntegral offsetVal
--     return driverFee

findWindowsWithStatus :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus (Id driverId) startTime endTime mbStatus limitVal offsetVal =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

-- findOngoingAfterEndTime :: Transactionable m => Id Person -> UTCTime -> m (Maybe DriverFee)
-- findOngoingAfterEndTime driverId now = do
--   findOne $ do
--     -- assuming one such entry only
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--         &&. driverFee ^. DriverFeeStatus ==. val ONGOING
--         &&. driverFee ^. DriverFeeEndTime <=. val now
--     return driverFee

findOngoingAfterEndTime :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq ONGOING,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findUnpaidAfterPayBy :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.payBy $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

-- updateFee :: Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> SqlDB ()
-- updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
--   let fare = fromMaybe 0 mbFare
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeGovtCharges +=. val govtCharges,
--         DriverFeePlatformFee +=. val platformFee,
--         DriverFeeCgst +=. val cgst,
--         DriverFeeSgst +=. val sgst,
--         DriverFeeStatus =. val ONGOING,
--         DriverFeeTotalEarnings +=. val fare,
--         DriverFeeNumRides +=. val 1, -- in the api, num_rides needed without cost contribution?
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateFee :: (L.MonadFlow m, Log m) => Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
  driverFeeObject <- findById driverFeeId
  case driverFeeObject of
    Just df -> do
      let govtCharges' = df.govtCharges
      let platformFee' = df.platformFee.fee
      let cgst' = df.platformFee.cgst
      let sgst' = df.platformFee.sgst
      let totalEarnings = df.totalEarnings
      let numRides = df.numRides
      let fare = fromMaybe 0 mbFare
      updateOneWithKV
        [ Se.Set BeamDF.govtCharges $ govtCharges' + govtCharges,
          Se.Set BeamDF.platformFee $ platformFee' + platformFee,
          Se.Set BeamDF.cgst $ cgst' + cgst,
          Se.Set BeamDF.sgst $ sgst' + sgst,
          Se.Set BeamDF.status ONGOING,
          Se.Set BeamDF.totalEarnings $ totalEarnings + fare,
          Se.Set BeamDF.numRides $ numRides + 1, -- in the api, num_rides needed without cost contribution?
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()

-- updateStatusByIds :: DriverFeeStatus -> [Id DriverFee] -> UTCTime -> SqlDB ()
-- updateStatusByIds status driverFeeId now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeStatus =. val status,
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeTId `in_` valList (toKey <$> driverFeeId)

updateStatusByIds :: (L.MonadFlow m, Log m) => DriverFeeStatus -> [Id DriverFee] -> UTCTime -> m ()
updateStatusByIds status driverFeeIds now =
  updateWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

-- findAllPendingAndDueDriverFeeByDriverId :: Transactionable m => Id Person -> m [DriverFee]
-- findAllPendingAndDueDriverFeeByDriverId driverId = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeFeeType ==. val RECURRING_INVOICE
--         &&. (driverFee ^. DriverFeeStatus ==. val PAYMENT_PENDING ||. driverFee ^. DriverFeeStatus ==. val PAYMENT_OVERDUE)
--         &&. driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--     return driverFee

findAllPendingAndDueDriverFeeByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m [DriverFee]
findAllPendingAndDueDriverFeeByDriverId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE, Se.Or [Se.Is BeamDF.status $ Se.Eq PAYMENT_OVERDUE, Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING], Se.Is BeamDF.driverId $ Se.Eq driverId]]

-- updateStatus :: DriverFeeStatus -> Id DriverFee -> UTCTime -> SqlDB ()
-- updateStatus status driverFeeId now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeStatus =. val status,
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateStatus :: (L.MonadFlow m, Log m) => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateRegisterationFeeStatusByDriverId :: (L.MonadFlow m, Log m, MonadTime m) => DriverFeeStatus -> Id Person -> m ()
updateRegisterationFeeStatusByDriverId status (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.And [Se.Is BeamDF.driverId (Se.Eq driverId), Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION), Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)]]

-- updateCollectedPaymentStatus :: DriverFeeStatus -> Maybe Text -> Id DriverFee -> UTCTime -> SqlDB ()
-- updateCollectedPaymentStatus status collectorId driverFeeId now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeStatus =. val status,
--         DriverFeeCollectedBy =. val collectorId,
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateCollectedPaymentStatus :: (L.MonadFlow m, Log m) => DriverFeeStatus -> Maybe Text -> UTCTime -> Id DriverFee -> m ()
updateCollectedPaymentStatus status collectorId now (Id driverFeeId) = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedBy collectorId]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

instance FromTType' BeamDF.DriverFee DriverFee where
  fromTType' BeamDF.DriverFeeT {..} = do
    pure $
      Just
        DriverFee
          { id = Id id,
            merchantId = Id merchantId,
            driverId = Id driverId,
            govtCharges = govtCharges,
            platformFee = Domain.PlatformFee platformFee cgst sgst,
            numRides = numRides,
            payBy = payBy,
            totalEarnings = totalEarnings,
            startTime = startTime,
            endTime = endTime,
            status = status,
            feeType = feeType,
            collectedBy = collectedBy,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamDF.DriverFee DriverFee where
  toTType' DriverFee {..} = do
    BeamDF.DriverFeeT
      { BeamDF.id = getId id,
        BeamDF.merchantId = getId merchantId,
        BeamDF.driverId = getId driverId,
        BeamDF.govtCharges = govtCharges,
        BeamDF.platformFee = platformFee.fee,
        BeamDF.cgst = platformFee.cgst,
        BeamDF.sgst = platformFee.sgst,
        BeamDF.numRides = numRides,
        BeamDF.payBy = payBy,
        BeamDF.totalEarnings = totalEarnings,
        BeamDF.startTime = startTime,
        BeamDF.endTime = endTime,
        BeamDF.status = status,
        BeamDF.feeType = feeType,
        BeamDF.collectedBy = collectedBy,
        BeamDF.createdAt = createdAt,
        BeamDF.updatedAt = updatedAt
      }
