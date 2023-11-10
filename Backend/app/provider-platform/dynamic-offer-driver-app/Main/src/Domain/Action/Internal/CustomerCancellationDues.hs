{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.CustomerCancellationDues where

import qualified Domain.Types.CancellationCharges as DCC
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD

data CancellationDuesReq = CancellationDuesReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { customerCancellationDues :: Money,
    disputeChancesUsed :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    cancellationCharges :: Maybe Money,
    disputeChancesUsed :: Maybe Int,
    paymentMadeToDriver :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

disputeCancellationDues ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  CancellationDuesReq ->
  m APISuccess
disputeCancellationDues merchantId apiKey CancellationDuesReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (RiderDetailsDoNotExist "Mobile Number" customerMobileNumber)

  when (riderDetails.cancellationDues == 0) $ do
    throwError $ InvalidRequest "Cancellation Due Amount is 0"
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchant.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.city)

  transporterConfig <- SCT.findByMerchantOpCityId merchantOperatingCity.id >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  when (riderDetails.disputeChancesUsed >= transporterConfig.cancellationFeeDisputeLimit) $ do
    throwError $ InvalidRequest "Customer can't dispute cancellation dues due to limits reached"
  let disputeChancesLeft = transporterConfig.cancellationFeeDisputeLimit - riderDetails.disputeChancesUsed
      disputeAmount = min (transporterConfig.cancellationFee * fromIntegral disputeChancesLeft) riderDetails.cancellationDues
      disputeChances = disputeAmount `div` transporterConfig.cancellationFee
  QRD.updateDisputeChancesUsedAndCancellationDues riderDetails.id (riderDetails.disputeChancesUsed + fromIntegral disputeChances) (riderDetails.cancellationDues - disputeAmount)
  pure Success

getCancellationDuesDetails ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  CancellationDuesReq ->
  m CancellationDuesDetailsRes
getCancellationDuesDetails merchantId apiKey CancellationDuesReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider Details Not Found")
  return $ CancellationDuesDetailsRes {customerCancellationDues = riderDetails.cancellationDues, disputeChancesUsed = riderDetails.disputeChancesUsed}

customerCancellationDuesSync ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  CustomerCancellationDuesSyncReq ->
  m APISuccess
customerCancellationDuesSync merchantId apiKey req = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash req.customerMobileNumber

  when (isJust req.cancellationCharges && isJust req.disputeChancesUsed) $ do
    throwError $ InvalidRequest "Either of the two , Due Amount or Dispute Chances has to be Null"
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchant.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.city)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOperatingCity.id >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider Details Not Found")
  case (req.cancellationCharges, req.disputeChancesUsed) of
    (Just amountPaid, Nothing) -> do
      when (amountPaid > riderDetails.cancellationDues || amountPaid < 0) $ do
        throwError $ InvalidRequest "Limits not met for the input request"
      when (req.paymentMadeToDriver) $ do
        booking <- (QBooking.findLastCancelledByRiderId riderDetails.id) >>= fromMaybeM (BookingDoesNotExist riderDetails.id.getId)
        ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = ride.driverId,
                  rideId = Just ride.id,
                  cancellationCharges = amountPaid,
                  ..
                }
        QCC.create cancellationCharges

      let disputeChances = fromIntegral $ amountPaid `div` transporterConfig.cancellationFee
      QRD.updateDisputeChancesUsedAndCancellationDues riderDetails.id (max 0 (riderDetails.disputeChancesUsed - disputeChances)) (riderDetails.cancellationDues - amountPaid)
    (Nothing, Just disputeChancesUsedReq) -> do
      when (disputeChancesUsedReq > transporterConfig.cancellationFeeDisputeLimit || disputeChancesUsedReq < 0) $ do
        throwError $ InvalidRequest "Limits not met for the dispute chances used request"
      when (riderDetails.disputeChancesUsed >= disputeChancesUsedReq) $ do
        QRD.updateDisputeChancesUsed riderDetails.id disputeChancesUsedReq
    (_, _) -> throwError $ InvalidRequest "One among Cancellation Charges or Dispute Chances Used should only be passed"
  return Success
