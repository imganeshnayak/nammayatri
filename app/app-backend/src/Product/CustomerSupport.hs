{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Product.Booking (buildBookingStatusRes)
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.CustomerSupport as T
import Types.Error
import Utils.Common

login :: T.LoginReq -> FlowHandler T.LoginRes
login T.LoginReq {..} = withFlowHandlerAPI $ do
  person <- Person.findByEmailAndPassword email password >>= fromMaybeM (PersonNotFound email)
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  token <- generateToken person
  pure $ T.LoginRes token "Logged in successfully"

generateToken :: EsqDBFlow m r => SP.Person -> m Text
generateToken SP.Person {..} = do
  let personId = id
  regToken <- createSupportRegToken $ getId personId
  -- Clean Old Login Session
  DB.runTransaction $ do
    RegistrationToken.deleteByPersonId personId
    RegistrationToken.create regToken
  pure $ regToken.token

logout :: Id SP.Person -> FlowHandler T.LogoutRes
logout personId = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  DB.runTransaction (RegistrationToken.deleteByPersonId person.id)
  pure $ T.LogoutRes "Logged out successfully"

createSupportRegToken :: MonadFlow m => Text -> m SR.RegistrationToken
createSupportRegToken entityId = do
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 1, -- Token
        authMedium = SR.EMAIL,
        authType = SR.PASSWORD,
        authValueHash = "CUSTOMER_SESSIONTOKEN",
        verified = True,
        authExpiry = 0,
        tokenExpiry = 30, -- Need to Make this Configuable
        entityId = entityId,
        entityType = SR.CUSTOMER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

listOrder :: Id SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [T.OrderResp]
listOrder personId mRequestId mMobile mlimit moffset = withFlowHandlerAPI $ do
  supportP <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (supportP.role == SP.CUSTOMER_SUPPORT) $
    throwError AccessDenied
  T.OrderInfo {person, bookings} <- case (mRequestId, mMobile) of
    (Just bookingId, _) -> getByRequestId bookingId supportP.merchantId
    (_, Just mobileNumber) -> getByMobileNumber mobileNumber supportP.merchantId
    (_, _) -> throwError $ InvalidRequest "You should pass SearchRequestId or mobile number."
  traverse (buildBookingToOrder person) bookings
  where
    getByMobileNumber number merchantId = do
      let limit = maybe 10 (\x -> if x <= 10 then x else 10) mlimit
      person <-
        Person.findByRoleAndMobileNumberAndMerchantIdWithoutCC SP.USER number merchantId
          >>= fromMaybeM (PersonDoesNotExist number)
      bookings <-
        QRB.findAllByPersonIdLimitOffset (person.id) (Just limit) moffset
      return $ T.OrderInfo person bookings
    getByRequestId bookingId merchantId = do
      (booking :: DRB.Booking) <-
        QRB.findByIdAndMerchantId (Id bookingId) merchantId
          >>= fromMaybeM (BookingDoesNotExist bookingId)
      let requestorId = booking.riderId
      person <-
        Person.findById requestorId
          >>= fromMaybeM (PersonDoesNotExist requestorId.getId)
      return $ T.OrderInfo person [booking]

buildBookingToOrder :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> DRB.Booking -> m T.OrderResp
buildBookingToOrder SP.Person {firstName, lastName, mobileNumber} booking = do
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
  rbStatus <- buildBookingStatusRes booking
  decMobNum <- mapM decrypt mobileNumber
  let details =
        T.OrderDetails
          { id = getId booking.id,
            createdAt = booking.createdAt,
            updatedAt = booking.updatedAt,
            startTime = booking.startTime,
            endTime = Nothing,
            fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
            toLocation = DBLoc.makeBookingLocationAPIEntity <$> mbToLocation,
            travellerName = firstName <> lastName,
            travellerPhone = decMobNum,
            rideBooking = rbStatus
          }
  pure $ T.OrderResp {order = details}
