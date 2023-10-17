{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide
  ( ServiceHandle (..),
    DriverEndRideReq (..),
    CallBasedEndRideReq (..),
    DashboardEndRideReq (..),
    CronJobEndRideReq (..),
    EndRideResp (..),
    callBasedEndRide,
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
    cronJobEndRide,
  )
where

import Data.OpenApi.Internal.Schema (ToSchema)
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTConf
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import Environment (Flow)
import EulerHS.Prelude hiding (id, pi)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude (roundToIntegral)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.DriverLocation as DrLoc
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.SMS as Sms

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq | CallBasedReq CallBasedEndRideReq | CronJobReq CronJobEndRideReq

data EndRideResp = EndRideResp
  { result :: Text,
    homeLocationReached :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverEndRideReq = DriverEndRideReq
  { point :: LatLong,
    requestor :: DP.Person,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    odometerEndReading :: Maybe Int,
    endRideOtp :: Maybe Text
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

data CronJobEndRideReq = CronJobEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

newtype CallBasedEndRideReq = CallBasedEndRideReq
  { requestor :: DP.Person
  }

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    getMerchant :: Id DM.Merchant -> m (Maybe DM.Merchant),
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> Id DM.Merchant -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> m (),
    getFarePolicy :: Id DM.Merchant -> DVeh.Variant -> Maybe DFareProduct.Area -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    findDriverLoc :: Id DP.Person -> m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m (),
    getDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> m Meters,
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DM.Merchant -> m (Maybe DMPM.MerchantPaymentMethod),
    sendDashboardSms :: Id DM.Merchant -> Sms.DashboardMessageType -> Maybe DRide.Ride -> Id DP.Person -> Maybe SRB.Booking -> HighPrecMoney -> m (),
    uiDistanceCalculation :: Id DRide.Ride -> Maybe Int -> Maybe Int -> m ()
  }

buildEndRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId True
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getMerchant = MerchantS.findById,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction,
        getFarePolicy = FarePolicy.getFarePolicy,
        calculateFareParameters = Fare.calculateFareParameters,
        putDiffMetric = RideEndInt.putDiffMetric,
        findDriverLoc = DrLoc.findById,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = QTConf.findByMerchantId merchantId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getDistanceBetweenPoints = RideEndInt.getDistanceBetweenPoints merchantId,
        findPaymentMethodByIdAndMerchantId = CQMPM.findByIdAndMerchantId,
        sendDashboardSms = Sms.sendDashboardSms,
        uiDistanceCalculation = QRide.updateUiDistanceCalculation
      }

type EndRideFlow m r = (MonadFlow m, CoreMetrics m, MonadReader r m, HasField "enableAPILatencyLogging" r Bool, HasField "enableAPIPrometheusMetricLogging" r Bool, LT.HasLocationService m r)

driverEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m EndRideResp
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m EndRideResp
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ DashboardReq req
  return APISuccess.Success

cronJobEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CronJobEndRideReq ->
  m APISuccess.APISuccess
cronJobEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ CronJobReq req
  return APISuccess.Success

endRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRide handle@ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) do
  rideOld <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)
  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case rideOld.rideDetails of
        DRide.RideDetailsOnDemand {} -> pure ()
        DRide.RideDetailsRental {} -> do
          when (isJust driverReq.odometerEndReading) $ do
            when (driverReq.endRideOtp /= rideOld.rideDetails.endRideOtp) $ throwError IncorrectOTP
      uiDistanceCalculation rideOld.id driverReq.uiDistanceCalculationWithAccuracy driverReq.uiDistanceCalculationWithoutAccuracy -- more checks?
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CronJobReq cronJobReq -> do
      unless (booking.providerId == cronJobReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CallBasedReq callBasedEndRideReq -> do
      let requestor = callBasedEndRideReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  (tripEndPoint, odometerEndReading) <-
    case rideOld.rideDetails of
      DRide.RideDetailsOnDemand {} -> do
        case req of
          DriverReq driverReq -> do
            logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            pure (driverReq.point, driverReq.odometerEndReading :: Maybe Int)
          DashboardReq dashboardReq -> do
            logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case dashboardReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                pure (getCoordinates booking.bookingDetails.toLocation, Nothing)
          CronJobReq cronJobReq -> do
            logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case cronJobReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                pure (getCoordinates booking.bookingDetails.toLocation, Nothing)
          CallBasedReq _ -> do
            pure (getCoordinates booking.bookingDetails.toLocation, Nothing)
      DRide.RideDetailsRental {} -> do
        case req of
          DriverReq driverReq -> do
            logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            pure (driverReq.point, driverReq.odometerEndReading :: Maybe Int)
          DashboardReq dashboardReq -> do
            logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case dashboardReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                error "TODO"
          CronJobReq cronJobReq -> do
            logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case cronJobReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                error "TODO"
          CallBasedReq _ -> do
            error "TODO"

  goHomeConfig <- CQGHC.findByMerchantId booking.providerId
  ghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId booking.providerId (Just goHomeConfig)

  homeLocationReached' <-
    case odometerEndReading of
      Nothing -> do
        if ghInfo.status == Just DDGR.ACTIVE && goHomeConfig.enableGoHome
          then do
            case ghInfo.driverGoHomeRequestId of
              Nothing -> do
                logError "DriverGoHomeRequestId not present even though status is active."
                return Nothing
              Just ghrId -> do
                mbDriverGoHomeReq <- QDGR.findById ghrId
                case mbDriverGoHomeReq of
                  Just driverGoHomeReq -> do
                    let driverHomeLocation = Maps.LatLong {lat = driverGoHomeReq.lat, lon = driverGoHomeReq.lon}
                    routesResp <- DMaps.getTripRoutes (driverId, booking.providerId) (buildRoutesReq tripEndPoint driverHomeLocation)
                    logDebug $ "Routes resp for EndRide API :" <> show routesResp <> "(source, dest) :" <> show (tripEndPoint, driverHomeLocation)
                    let driverHomeDists = mapMaybe (.distance) routesResp
                    if any ((<= goHomeConfig.destRadiusMeters) . getMeters) driverHomeDists
                      then do
                        CQDGR.deactivateDriverGoHomeRequest booking.providerId driverId DDGR.SUCCESS ghInfo (Just True)
                        return $ Just True
                      else do
                        CQDGR.resetDriverGoHomeRequest booking.providerId driverId goHomeConfig ghInfo
                        return $ Just False
                  Nothing -> return Nothing
          else return Nothing
      _ -> return Nothing

  whenWithLocationUpdatesLock driverId $ do
    thresholdConfig <- findConfig >>= fromMaybeM (InternalError "TransportConfigNotFound")
    now <- getCurrentTime
    (chargeableDistance, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case odometerEndReading of
        Nothing -> do
          case req of
            CronJobReq _ -> do
              logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
              (chargeableDistance, finalFare, mbUpdatedFareParams) <- recalculateFareForDistance handle booking rideOld booking.estimatedDistance thresholdConfig.timeDiffFromUtc
              pure (chargeableDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
            _ -> do
              -- here we update the current ride, so below we fetch the updated version
              pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
              enableLocationTrackingService <- asks (.enableLocationTrackingService)
              tripEndPoints <-
                if enableLocationTrackingService
                  then do
                    res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId
                    pure $ toList res.loc
                  else pure [tripEndPoint]
              whenJust (nonEmpty tripEndPoints) \tripEndPoints' -> do
                withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rideOld.id driverId tripEndPoints' booking.estimatedDistance pickupDropOutsideOfThreshold

              ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

              distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId
              when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId
              (chargeableDistance, finalFare, mbUpdatedFareParams) <-
                if distanceCalculationFailed
                  then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                  else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig
              pure (chargeableDistance, finalFare, mbUpdatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
        Just endReading -> do
          farePolicy <- getFarePolicy booking.providerId booking.vehicleVariant booking.area
          logInfo $ "farePolicia :" <> show farePolicy
          (recalcDistance, finalFare, mbUpdatedFareParams) <- case farePolicy.farePolicyDetails of
            DFP.ProgressiveDetails _ -> throwError $ InternalError "Rental does not support progressive FP"
            DFP.SlabsDetails _ -> throwError $ InternalError "Rental does not support slab FP"
            DFP.RentalDetails _ -> recalculateFareForDistance handle booking rideOld (Meters $ (endReading - fromMaybe 0 rideOld.rideDetails.odometerStartReading) * 1000) thresholdConfig.timeDiffFromUtc
          ride <- QRide.findById (cast rideOld.id) >>= fromMaybeM (RideDoesNotExist rideOld.id.getId)
          pure (recalcDistance, finalFare, mbUpdatedFareParams, ride, Nothing, Nothing)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just tripEndPoint,
               fareParametersId = Just newFareParams.id,
               distanceCalculationFailed = distanceCalculationFailed,
               pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               rideDetails = case rideOld.rideDetails of
                 DRide.RideDetailsOnDemand {} -> rideOld.rideDetails
                 DRide.RideDetailsRental {} -> rideOld.rideDetails{odometerEndReading = odometerEndReading}
              }
    -- we need to store fareParams only when they changed
    withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig booking.providerId
    withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.providerId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentUrl = DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo mbPaymentUrl

    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId Sms.ENDRIDE (Just ride) driverId (Just booking) (fromIntegral finalFare)
        _ -> pure ()

  return $ EndRideResp {result = "Success", homeLocationReached = homeLocationReached'}
  where
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> Seconds -> m (Meters, Money, Maybe FareParameters)
recalculateFareForDistance ServiceHandle {..} booking ride recalcDistance timeDiffFromUtc = do
  let merchantId = booking.providerId
      oldDistance = booking.estimatedDistance

  now <- getLocalCurrentTime timeDiffFromUtc
  actualDuration <- case ride.tripStartTime of
    Nothing -> throwError $ InternalError "No start reading found" -- Impossible case
    Just startTime -> pure $ round (now `diffUTCTime` addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) startTime) `div` 3600
  -- maybe compare only distance fare?
  let estimatedFare = Fare.fareSum booking.fareParams
  farePolicy <- getFarePolicy merchantId booking.vehicleVariant booking.area
  fareParams <- case ride.rideDetails of
    DRide.RideDetailsOnDemand {} ->
      calculateFareParameters
        Fare.CalculateFareParametersParams
          { farePolicy = farePolicy,
            distance = recalcDistance,
            rideTime = booking.startTime,
            waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
            driverSelectedFare = booking.fareParams.driverSelectedFare,
            customerExtraFee = booking.fareParams.customerExtraFee,
            nightShiftCharge = booking.fareParams.nightShiftCharge, -- TODO: Make below checks on bookingType
            rideStartTime = Nothing, --if isJust ride.odometerStartReading then addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) <$> ride.tripStartTime else Nothing,
            rideEndTime = Nothing, --if isJust ride.odometerStartReading then Just now else Nothing,
            actualDistance = Nothing, --if isJust ride.odometerStartReading then Just (recalcDistance.getMeters `div` 1000) else Nothing,
            chargedDuration = 0, --if isJust ride.odometerStartReading then max actualDuration (booking.estimatedDuration.getSeconds `div` 3600) else 0,
            nightShiftOverlapChecking = Just False, --Just $ isJust ride.odometerStartReading,
            now = now,
            endRideTime = Just now
          }
    DRide.RideDetailsRental {odometerStartReading} ->
      calculateFareParameters
        Fare.CalculateFareParametersParams
          { farePolicy = farePolicy,
            distance = recalcDistance,
            rideTime = booking.startTime,
            waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
            driverSelectedFare = booking.fareParams.driverSelectedFare,
            customerExtraFee = booking.fareParams.customerExtraFee,
            nightShiftCharge = booking.fareParams.nightShiftCharge, -- TODO: Make below checks on bookingType
            rideStartTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) <$> ride.tripStartTime,
            rideEndTime = Just now,
            actualDistance = Just (recalcDistance.getMeters `div` 1000),
            chargedDuration = max actualDuration (booking.estimatedDuration.getSeconds `div` 3600),
            nightShiftOverlapChecking = Just $ isJust odometerStartReading,
            now = now,
            endRideTime = Just now
          }
  let finalFare = Fare.fareSum fareParams
      distanceDiff = recalcDistance - oldDistance
      fareDiff = finalFare - estimatedFare
  logTagInfo "Fare recalculation" $
    "Fare difference: "
      <> show (realToFrac @_ @Double fareDiff)
      <> ", Distance difference: "
      <> show distanceDiff
  putDiffMetric merchantId fareDiff distanceDiff
  return (recalcDistance, finalFare, Just fareParams)

isPickupDropOutsideOfThreshold :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> DRide.Ride -> LatLong -> DTConf.TransporterConfig -> m Bool
isPickupDropOutsideOfThreshold booking ride tripEndPoint thresholdConfig = do
  let mbTripStartLoc = ride.tripStartPos
  -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
  case mbTripStartLoc of
    Nothing -> pure True
    Just tripStartLoc -> do
      let pickupLocThreshold = metersToHighPrecMeters thresholdConfig.pickupLocThreshold
      let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      let pickupDifference = abs $ distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
      let dropDifference = abs $ distanceBetweenInMeters (getCoordinates booking.bookingDetails.toLocation) tripEndPoint
      let pickupDropOutsideOfThreshold = (pickupDifference >= pickupLocThreshold) || (dropDifference >= dropLocThreshold)

      logTagInfo "Locations differences" $
        "Pickup difference: "
          <> show pickupDifference
          <> ", Drop difference: "
          <> show dropDifference
          <> ", Locations outside of thresholds: "
          <> show pickupDropOutsideOfThreshold
      pure pickupDropOutsideOfThreshold

getDistanceDiff :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> Meters -> m HighPrecMeters
getDistanceDiff booking distance = do
  let rideDistanceDifference = distance - booking.estimatedDistance
  logTagInfo "RideDistance differences" $
    "Distance Difference: "
      <> show rideDistanceDifference
  pure $ metersToHighPrecMeters rideDistanceDifference

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + thresholdConfig.actualRideDistanceDiffThreshold
  if not pickupDropOutsideOfThreshold
    then
      if distanceDiff > thresholdConfig.actualRideDistanceDiffThreshold
        then recalculateFareForDistance handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig.timeDiffFromUtc
        else recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig.timeDiffFromUtc
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig.timeDiffFromUtc
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig.timeDiffFromUtc
            else recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig.timeDiffFromUtc

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  approxTraveledDistance <- getDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints
  logTagInfo "endRide" $ "approxTraveledDistance: " <> show approxTraveledDistance
  distanceDiff <- getDistanceDiff booking approxTraveledDistance

  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig.timeDiffFromUtc
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig.timeDiffFromUtc
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig.timeDiffFromUtc
            else do
              if distanceDiff < thresholdConfig.upwardsRecomputeBuffer
                then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig.timeDiffFromUtc
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistance handle booking ride (booking.estimatedDistance + highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer) thresholdConfig.timeDiffFromUtc

-- calcRentalFare :: MonadFlow m => Seconds -> Int -> DFP.FPRentalDetailsD s -> DRide.Ride -> Seconds -> UTCTime -> Maybe DFP.NightShiftBounds -> m (Meters, Money, Maybe FareParameters, DRide.Ride, Maybe Bool, Maybe Bool)
-- calcRentalFare estimatedDuration endReading rd rideOld timeDiffFromUtc now nightShiftBounds = do
--   actualDistance <- case rideOld.odometerStartReading of
--     Nothing -> throwError $ InternalError "No start reading found" -- Impossible case
--     Just startReading -> pure (endReading - startReading)
--   (actualDuration, tripStartTime) <- case rideOld.tripStartTime of
--     Nothing -> throwError $ InternalError "No start time found" -- Impossible case
--     Just startTime -> pure (round (now `diffUTCTime` startTime) `div` 3600, startTime)
--   let chargedDuration = max actualDuration (estimatedDuration.getSeconds `div` 60)
--       fareByTime = Money $ rd.baseFare.getMoney + (chargedDuration * rd.perHourCharge.getMoney)
--       extraKm = actualDistance - (chargedDuration * rd.perHourFreeKms)
--       distanceBuffer = DFP.findFPRentalDetailsByDuration chargedDuration rd.distanceBuffers
--       fareByDistAndTime = if extraKm >= distanceBuffer.bufferKms then Money (fareByTime.getMoney + extraKm * rd.perExtraKmRate.getMoney) else fareByTime
--       rideEndDate = utctDay (addUTCTime (fromIntegral timeDiffFromUtc) now)
--       nightAllowance = isNightAllowanceApplicable nightShiftBounds rideEndDate (addUTCTime (fromIntegral timeDiffFromUtc) tripStartTime) (addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now)
--       totalFare = if nightAllowance then fareByDistAndTime + rd.nightShiftAllowance else fareByDistAndTime
--   ride <- QRide.findById (cast rideOld.id) >>= fromMaybeM (RideDoesNotExist rideOld.id.getId)
--   logDebug $
--     "Fare by time :"
--       <> show fareByTime
--       <> ", Fare by dist and time :"
--       <> show fareByDistAndTime
--       <> ", Night allowance: "
--       <> show nightAllowance
--   pure (Meters actualDistance * 1000, totalFare, Nothing, ride, Nothing, Nothing)

-- isNightAllowanceApplicable :: Maybe DFP.NightShiftBounds -> Day -> UTCTime -> UTCTime -> Bool
-- isNightAllowanceApplicable nightShiftBounds rideEndDate tripStartTime now = do
--   case nightShiftBounds of
--     Nothing -> False
--     Just bounds -> do
--       let nightShiftStartTime = timeOfDayToDiffTime bounds.nightShiftStart
--           nightShiftEndTime = timeOfDayToDiffTime bounds.nightShiftEnd
--       if nightShiftStartTime <= 6 * 60 * 60 -- NS starting and ending on same date
--         then isNightShift rideEndDate nightShiftStartTime nightShiftEndTime tripStartTime now 0 0
--         else isNightShift rideEndDate nightShiftStartTime nightShiftEndTime tripStartTime now (-1) 0 || isNightShift rideEndDate nightShiftStartTime nightShiftEndTime tripStartTime now 0 1

-- isNightShift :: Day -> DiffTime -> DiffTime -> UTCTime -> UTCTime -> Integer -> Integer -> Bool
-- isNightShift rideEndDate nightShiftStartTime nightShiftEndTime tripStartTime now startAdd endAdd = do
--   let curNightShiftStartTs = UTCTime (addDays startAdd rideEndDate) nightShiftStartTime
--       curNightShiftEndTs = UTCTime (addDays endAdd rideEndDate) nightShiftEndTime
--       curMxStart = max curNightShiftStartTs tripStartTime
--       curMnEnd = min curNightShiftEndTs now
--   curMnEnd >= curMxStart

-- timeOfDayToDiffTime :: TimeOfDay -> DiffTime -- TODO :  Move to Kernel
-- timeOfDayToDiffTime (TimeOfDay h m s) = secondsToDiffTime $ fromIntegral (h * 3600 + m * 60 + floor s)
