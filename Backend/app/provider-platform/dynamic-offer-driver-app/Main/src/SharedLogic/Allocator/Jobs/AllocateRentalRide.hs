{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.AllocateRentalRide where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import qualified Tools.Metrics as Metrics

allocateRentalRide ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    Log m,
    MonadFlow m,
    LT.HasLocationService m r
  ) =>
  Job 'AllocateRentalRide ->
  m ExecutionResult
allocateRentalRide Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  searchReq <- B.runInReplica $ QSR.findById jobData.searchRequestId >>= fromMaybeM (SearchRequestNotFound jobData.searchRequestId.getId)
  booking <- QB.findById jobData.bookingId >>= fromMaybeM (BookingNotFound jobData.bookingId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig merchant.id booking.estimatedDistance
  goHomeCfg <- CQGHC.findByMerchantId merchant.id
  (res, _) <- sendSearchRequestToDrivers' driverPoolConfig searchReq booking merchant Nothing goHomeCfg
  return res

-- TODO remove redundant constraints everywhere:
-- EsqDBFlow m r,
-- EsqLocDBFlow m r,
-- EsqLocRepDBFlow m r,

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    Log m,
    LT.HasLocationService m r
  ) =>
  DriverPoolConfig ->
  SearchRequest ->
  DB.Booking ->
  Merchant ->
  Maybe DFP.DriverExtraFeeBounds ->
  GoHomeConfig ->
  m (ExecutionResult, Bool)
sendSearchRequestToDrivers' driverPoolConfig searchReq booking merchant driverExtraFeeBounds goHomeCfg = do
  handler handle goHomeCfg
  where
    searchId = cast @SearchRequest @DSRD.Search searchReq.id
    searchDetails = DSRD.RentalSearchDetails {booking}
    handle =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchId,
          isReceivedMaxDriverQuotes = pure $ booking.status /= DB.SCHEDULED,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig searchReq searchId booking.vehicleVariant,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq searchDetails driverExtraFeeBounds driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          setBatchDurationLock = I.setBatchDurationLock searchId driverPoolConfig.singleBatchProcessTime,
          createRescheduleTime = I.createRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              },
          isSearchTryValid = pure True,
          cancelSearchTry = pure () -- I.cancelSearchTry bookingId -- FIXME cancelBooking ??
        }
