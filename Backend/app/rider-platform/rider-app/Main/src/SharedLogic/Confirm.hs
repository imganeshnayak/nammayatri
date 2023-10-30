{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalDetails
import qualified Domain.Types.SearchRequest as DSReq
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error
import Tools.Event

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quoteId :: Id DQuote.Quote,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    mbStartTime :: Maybe UTCTime,
    mbRentalDuration :: Maybe Int
  }

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    booking :: DRB.Booking,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    mbStartTime :: Maybe UTCTime,
    mbRentalDuration :: Maybe Int
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalDetailsAPIEntity
  | ConfirmAutoDetails Text (Maybe Text)
  | ConfirmOneWaySpecialZoneDetails Text
  deriving (Show, Generic)

calculateRentalEstimateFare :: Int -> RentalDetails -> Money
calculateRentalEstimateFare duration RentalDetails {..} = baseFare + perHourCharge * Money duration + nightShiftCharge

confirm ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  fulfillmentId <-
    case quote.quoteDetails of
      DQuote.OneWayDetails _ -> pure Nothing
      DQuote.RentalDetails rentalDetails -> do
        startTime <- mbStartTime & fromMaybeM (InvalidRequest "Rental confirm quote should have startTime param")
        rentalDuration <- mbRentalDuration & fromMaybeM (InvalidRequest "Rental confirm quote should have rentalDuration param")
        checkRentalBookingsOverlapping startTime rentalDuration
        let estimateFare = calculateRentalEstimateFare rentalDuration rentalDetails
        _ <- QQuote.updateQuoteEstimateFare quoteId estimateFare
        pure $ Just rentalDetails.id.getId
      DQuote.DriverOfferDetails driverOffer -> do
        estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
        when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
        when (driverOffer.validTill < now) $
          throwError $ QuoteExpired quote.id.getId
        pure (Just estimate.bppEstimateId.getId)
      DQuote.OneWaySpecialZoneDetails details -> pure (Just details.quoteId)
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatusObj personId DRB.activeBookingStatusObj
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  updatedFareQuote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
      driverId = getDriverId updatedFareQuote.quoteDetails
  exophone <- findRandomExophone searchRequest.merchantId
  booking <- buildBooking searchRequest fulfillmentId updatedFareQuote fromLocation mbToLocation exophone now mbStartTime Nothing paymentMethodId driverId mbRentalDuration
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderPhone <- mapM decrypt person.mobileNumber
  let riderName = person.firstName
  triggerBookingCreatedEvent BookingEventData {booking = booking}
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  details <- mkConfirmQuoteDetails updatedFareQuote.quoteDetails fulfillmentId
  paymentMethod <- forM paymentMethodId $ \paymentMethodId' -> do
    paymentMethod <-
      CQMPM.findByIdAndMerchantId paymentMethodId' searchRequest.merchantId
        >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId'.getId)
    unless (paymentMethodId' `elem` searchRequest.availablePaymentMethods) $
      throwError (InvalidRequest "Payment method not allowed")
    pure paymentMethod

  -- DB.runTransaction $ do
  _ <- QRideB.createBooking booking
  _ <- QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
  _ <- QEstimate.updateStatusByRequestId updatedFareQuote.requestId DEstimate.COMPLETED
  QPFS.clearCache searchRequest.riderId
  return $
    DConfirmRes
      { booking,
        providerId = updatedFareQuote.providerId,
        providerUrl = updatedFareQuote.providerUrl,
        itemId = updatedFareQuote.itemId,
        fromLoc = fromLocation,
        toLoc = mbToLocation,
        vehicleVariant = updatedFareQuote.vehicleVariant,
        quoteDetails = details,
        searchRequestId = searchRequest.id,
        maxEstimatedDistance = searchRequest.maxDistance,
        paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> paymentMethod,
        ..
      }
  where
    mkConfirmQuoteDetails quoteDetails fulfillmentId = do
      case quoteDetails of
        DQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
        DQuote.RentalDetails RentalDetails {..} -> do
          baseDuration <- Just . Hours <$> mbRentalDuration & fromMaybeM (InvalidRequest "Rental confirm quote should have rentalDuration param")
          pure $ ConfirmRentalDetails $ RentalDetailsAPIEntity {bppQuoteId = id.getId, ..}
        DQuote.DriverOfferDetails driverOffer -> do
          bppEstimateId <- fulfillmentId & fromMaybeM (InternalError "FulfillmentId not found in Init. this error should never come.")
          pure $ ConfirmAutoDetails bppEstimateId driverOffer.driverId
        DQuote.OneWaySpecialZoneDetails details -> pure $ ConfirmOneWaySpecialZoneDetails details.quoteId
    getDriverId :: DQuote.QuoteDetails -> Maybe Text
    getDriverId = \case
      DQuote.DriverOfferDetails driverOffer -> driverOffer.driverId
      _ -> Nothing

    checkRentalBookingsOverlapping startTime rentalDuration = do
      let statusObj =
            DRB.BookingStatusObj
              { normalBooking = [],
                rentalBooking = [DRB.NEW, DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.REALLOCATED, DRB.TRIP_ASSIGNED]
              }
      rentalBookings <- QRideB.findByRiderIdAndStatusObj personId statusObj
      let durationDiffTime = secondsToNominalDiffTime $ Seconds (rentalDuration * 3600)
      let finishTime = addUTCTime durationDiffTime startTime
      overlappedBookings <- flip filterM rentalBookings $ \booking -> do
        bookingRentalDuration <- case booking.bookingDetails of
          DRB.RentalDetails (DRB.BaseDuration baseDuration) _ -> pure baseDuration
          _ -> throwError (InternalError "Should be rental booking")
        let bookingDurationDiffTime = secondsToNominalDiffTime $ Seconds (bookingRentalDuration.getHours * 3600)
            bookingFinishTime = addUTCTime bookingDurationDiffTime booking.startTime
            startTimeOverlap = booking.startTime >= startTime && booking.startTime <= finishTime
            finishTimeOverlap = bookingFinishTime >= startTime && bookingFinishTime <= finishTime
        pure $ startTimeOverlap || finishTimeOverlap

      unless (null overlappedBookings) $ throwError (InvalidRequest $ "Overlapped rental bookings: " <> show (overlappedBookings <&> (.id)))

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  Maybe Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  Maybe Text ->
  Maybe Int ->
  m DRB.Booking
buildBooking searchRequest mbFulfillmentId quote fromLoc mbToLoc exophone now startTime otpCode paymentMethodId driverId mbRentalDuration = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        driverId,
        fulfillmentId = mbFulfillmentId,
        quoteId = Just quote.id,
        paymentMethodId,
        paymentUrl = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        primaryExophone = exophone.primaryPhone,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
        itemId = quote.itemId,
        providerMobileNumber = quote.providerMobileNumber,
        startTime = fromMaybe now startTime,
        riderId = searchRequest.riderId,
        fromLocation = fromLoc,
        estimatedFare = quote.estimatedFare,
        discount = quote.discount,
        estimatedTotalFare = quote.estimatedTotalFare,
        vehicleVariant = quote.vehicleVariant,
        bookingDetails,
        tripTerms = quote.tripTerms,
        merchantId = searchRequest.merchantId,
        specialLocationTag = quote.specialLocationTag,
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalDetails -> do
        rentalDuration <- mbRentalDuration & fromMaybeM (InvalidRequest "Rental confirm quote should have rentalDuration param")
        pure $ DRB.RentalDetails (DRB.BaseDuration $ Hours rentalDuration) rentalDetails
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  exophones <- CQExophone.findByMerchantAndService merchantId merchantServiceUsageConfig.getExophone
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones
