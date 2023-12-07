{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnCancel
  ( onCancel,
    validateRequest,
    OnCancelReq (..),
  )
where

import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.Notifications as Notify

data OnCancelReq = OnCancelReq
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    cancellationSource :: SBCR.CancellationSource
  }

data ValidatedOnCancelReq = ValidatedOnCancelReq
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    cancellationSource :: SBCR.CancellationSource,
    booking :: SRB.Booking,
    ride :: SRide.Ride,
    searchReq :: DSR.SearchRequest,
    estimate :: DEstimate.Estimate
  }

onCancel ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedOnCancelReq ->
  m ()
onCancel ValidatedOnCancelReq {..} = do
  let bookingCancellationReason = mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
  logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."
  _ <- QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
  _ <- QRB.updateStatus booking.id SRB.REALLOCATED
  _ <- QRide.updateStatus ride.id SRide.CANCELLED
  _ <- QBCR.upsert bookingCancellationReason
  _ <- QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
  QPFS.clearCache searchReq.riderId
  -- notify customer
  Notify.notifyOnEstimatedReallocated booking estimate.id
  let merchantOperatingCityId = searchReq.merchantOperatingCityId
  merchant <- QM.findById searchReq.merchantId >>= fromMaybeM (MerchantNotFound searchReq.merchantId.getId)
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  Redis.whenWithLockRedis (selectEstimateLockKey searchReq.riderId) 60 $ do
    let dSelectRes = mkSelectRes searchReq.customerExtraFee searchReq searchReq.autoAssignEnabled estimate merchant city
    becknReq <- ACL.buildSelectReq dSelectRes
    logTagInfo "Sending reallocated select request to bpp " $ show becknReq
    void $ withShortRetry $ CallBPP.select dSelectRes.providerUrl becknReq
  where
    mkSelectRes :: Maybe Money -> DSR.SearchRequest -> Maybe Bool -> DEstimate.Estimate -> DMerchant.Merchant -> Context.City -> DSelect.DSelectRes
    mkSelectRes customerExtraFee searchRequest autoAssignEnabled estimate' merchant city =
      DSelect.DSelectRes
        { estimate = estimate',
          providerId = estimate'.providerId,
          providerUrl = estimate'.providerUrl,
          variant = estimate'.vehicleVariant,
          autoAssignEnabled = isJust autoAssignEnabled && fromMaybe False autoAssignEnabled,
          ..
        }

selectEstimateLockKey :: Id DP.Person -> Text
selectEstimateLockKey personId = "Customer:SelectEstimate:CustomerId-" <> personId.getId

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c
  ) =>
  OnCancelReq ->
  m ValidatedOnCancelReq
validateRequest OnCancelReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
  return $ ValidatedOnCancelReq {..}

mkBookingCancellationReason ::
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  SBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
  SBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      merchantId = Just merchantId,
      source = cancellationSource,
      reasonCode = Nothing,
      reasonStage = Nothing,
      additionalInfo = Nothing,
      driverCancellationLocation = Nothing,
      driverDistToPickup = Nothing
    }
