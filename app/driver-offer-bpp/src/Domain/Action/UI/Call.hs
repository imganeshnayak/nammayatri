module Domain.Action.UI.Call
  ( CallCallbackRes,
    GetCallStatusRes,
    MobileNumberResp,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getCallStatus,
  )
where

import Beckn.External.Encryption (decrypt, getDbHash)
import Beckn.External.Exotel.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto (EsqDBReplicaFlow, runInReplica, runTransaction)
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Domain.Types.CallStatus as SCS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

type CallCallbackRes = AckResponse

type GetCallStatusRes = SCS.CallStatusAPIEntity

type MobileNumberResp = Text

directCallStatusCallback :: EsqDBFlow m r => Text -> Text -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> m MobileNumberResp
getCustomerMobileNumber callSid callFrom_ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  mobileNumberHash <- getDbHash callFrom
  driver <- runInReplica $ QPerson.findByMobileNumber "+91" mobileNumberHash >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica $ QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- runInReplica $ QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica $
      QRD.findById riderId
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id callId callSid callStatus
  runTransaction $ QCallStatus.create callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: (EsqDBFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity
