{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import Data.Aeson (object)
import Data.Default.Class
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Estimate (Estimate)
import qualified Domain.Types.Estimate as DEst
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Domain.Types.Person as Person
import Domain.Types.Quote (makeQuoteAPIEntity)
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as SRide
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Kernel.External.Notification as Notification
import Kernel.Storage.Esqueleto
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Metrics

data EmptyDynamicParam = EmptyDynamicParam

instance ToJSON EmptyDynamicParam where
  toJSON EmptyDynamicParam = object []

notifyPerson ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    Default a,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Notification.NotificationReq a b ->
  m ()
notifyPerson = runWithServiceConfig Notification.notifyPerson (.notifyPerson)

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  (Notification.NotificationServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> Notification.NotificationService) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId req = do
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  merchantNotificationServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.NotificationService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "notification" (show $ getCfg merchantConfig))
  case merchantNotificationServiceConfig.serviceConfig of
    DMSC.NotificationServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown ServiceConfig"

notifyOnDriverOfferIncoming ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Estimate ->
  [DQuote.Quote] ->
  Person.Person ->
  m ()
notifyOnDriverOfferIncoming estimateId quotes person = do
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_QUOTE_INCOMING,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product estimateId.getId $ map makeQuoteAPIEntity quotes,
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = "New driver offers incoming!"
      body =
        unwords
          [ "There are new driver offers!",
            "Check the app for details"
          ]
  notifyPerson person.merchantId notificationData

newtype RideAssignedParam = RideAssignedParam
  { driverName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideAssigned ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_ASSIGNMENT,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideAssignedParam driverName,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Driver assigned!"
      body =
        unwords
          [ driverName,
            "will be your driver for this trip."
          ]
  notifyPerson person.merchantId notificationData

newtype RideStartedParam = RideStartedParam
  { driverName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideStarted ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.TRIP_STARTED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideStartedParam driverName,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Trip started!"
      body =
        unwords
          [ driverName,
            "has started your trip. Please enjoy the ride!"
          ]
  notifyPerson person.merchantId notificationData

data RideCompleteParam = RideCompleteParam
  { driverName :: Text,
    fare :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideCompleted ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideCompleted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
      totalFare = ride.totalFare
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.TRIP_FINISHED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideCompleteParam driverName $ show (fromMaybe booking.estimatedFare totalFare),
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Trip finished!"
      body =
        unwords
          [ "Hope you enjoyed your trip with",
            driverName,
            "Total Fare " <> show (fromMaybe booking.estimatedFare totalFare)
          ]
  notifyPerson person.merchantId notificationData

notifyOnExpiration ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.riderId
  person <- Person.findById personId
  case person of
    Just p -> do
      let notificationData =
            Notification.NotificationReq
              { category = Notification.EXPIRED_CASE,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.SearchRequest searchRequestId.getId (),
                body = body,
                title = title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth p.id.getId p.deviceToken p.notificationToken
              }
          title = T.pack "Ride expired!"
          body =
            unwords
              [ "Your ride has expired as you did not confirm any offer.",
                "Please book again to continue."
              ]
      notifyPerson p.merchantId notificationData
    _ -> pure ()

notifyOnRegistration ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  RegistrationToken ->
  Person ->
  Maybe Text ->
  m ()
notifyOnRegistration regToken person mbDeviceToken = do
  let tokenId = RegToken.id regToken
      notificationData =
        Notification.NotificationReq
          { category = Notification.REGISTRATION_APPROVED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Merchant tokenId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId mbDeviceToken person.notificationToken
          }
      title = T.pack "Registration Completed!"
      body =
        unwords
          [ "Welcome to Yatri.",
            "Click here to book your first ride with us."
          ]
   in notifyPerson person.merchantId notificationData

newtype RideCancelParam = RideCancelParam
  { rideTime :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnBookingCancelled ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  SBCR.CancellationSource ->
  m ()
notifyOnBookingCancelled booking cancellationSource = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notifyPerson person.merchantId (notificationData booking.providerName person)
  where
    notificationData orgName person =
      Notification.NotificationReq
        { category = Notification.CANCELLED_PRODUCT,
          subCategory = Just subCategory,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product booking.id.getId (),
          body = getCancellationText orgName,
          title = title,
          dynamicParams = RideCancelParam $ showTimeIst (booking.startTime),
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
        }
    title = T.pack "Ride cancelled!"
    subCategory = case cancellationSource of
      SBCR.ByUser -> Notification.ByUser
      SBCR.ByMerchant -> Notification.ByMerchant
      SBCR.ByDriver -> Notification.ByDriver
      SBCR.ByAllocator -> Notification.ByAllocator
      SBCR.ByApplication -> Notification.ByApplication
    getCancellationText orgName = case cancellationSource of
      SBCR.ByUser ->
        unwords
          [ "You have cancelled your ride for",
            showTimeIst (booking.startTime) <> ".",
            "Check the app for details."
          ]
      SBCR.ByMerchant ->
        unwords
          [ "\"" <> orgName <> "\" agency had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByDriver ->
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByAllocator ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled as we could not find a driver.",
            "Please book again to get another ride."
          ]
      SBCR.ByApplication ->
        unwords
          [ "Sorry your ride for",
            showTimeIst (booking.startTime),
            "was cancelled.",
            "Please try to book again"
          ]

notifyOnBookingReallocated ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  m ()
notifyOnBookingReallocated booking = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notifyPerson person.merchantId (notificationData person)
  where
    notificationData person =
      Notification.NotificationReq
        { category = Notification.REALLOCATE_PRODUCT,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product booking.id.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
        }
    title = T.pack "Ride cancelled! We are allocating another driver"
    body =
      unwords
        [ "The driver had to cancel the ride for",
          showTimeIst (booking.startTime) <> ".",
          "Please wait until we allocate other driver."
        ]

notifyOnEstimatedReallocated ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  Id DEst.Estimate ->
  m ()
notifyOnEstimatedReallocated booking estimateId = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notifyPerson person.merchantId (notificationData person)
  where
    notificationData person =
      Notification.NotificationReq
        { category = Notification.REALLOCATE_PRODUCT,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product estimateId.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
        }
    title = T.pack "Ride cancelled!"
    body =
      unwords
        [ "The driver had to cancel the ride for",
          showTimeIst (booking.startTime) <> ".",
          "Please wait until we allocate other driver."
        ]

notifyOnQuoteReceived ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  DQuote.Quote ->
  m ()
notifyOnQuoteReceived quote = do
  searchRequest <- QSearchReq.findById quote.requestId >>= fromMaybeM (SearchRequestDoesNotExist quote.requestId.getId)
  person <- Person.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  let notificationData = mkNotificationData person
  notifyPerson person.merchantId notificationData
  where
    mkNotificationData person = do
      let title = T.pack "Quote received!"
          body =
            unwords
              [ "New quote received with price",
                show quote.estimatedFare <> "."
              ]
      Notification.NotificationReq
        { category = Notification.QUOTE_RECEIVED,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product quote.requestId.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
        }

notifyDriverOnTheWay ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  m ()
notifyDriverOnTheWay personId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_ON_THE_WAY,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product personId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Driver On The Way!"
      body =
        unwords
          [ "Driver is on the way"
          ]
  notifyPerson person.merchantId notificationData

data DriverReachedParam = DriverReachedParam
  { vehicleNumber :: Text,
    rideOtp :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyDriverHasReached ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  Text ->
  Text ->
  m ()
notifyDriverHasReached personId otp vehicleNumber = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_HAS_REACHED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product personId.getId (),
            body = body,
            title = title,
            dynamicParams = DriverReachedParam vehicleNumber otp,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Driver Has Reached!"
      body =
        unwords
          [ "Use OTP " <> otp <> " to verify the ride with Vehicle No. " <> vehicleNumber
          ]
  notifyPerson person.merchantId notificationData

notifyOnNewMessage ::
  ( HasCacheConfig r,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  T.Text ->
  m ()
notifyOnNewMessage booking message = do
  person <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let notificationData =
        Notification.NotificationReq
          { category = Notification.CHAT_MESSAGE,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product person.id.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
          }
      title = T.pack "Driver"
      body =
        unwords
          [ message
          ]
  notifyPerson person.merchantId notificationData
