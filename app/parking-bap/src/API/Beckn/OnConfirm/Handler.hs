module API.Beckn.OnConfirm.Handler where

import qualified API.Beckn.OnConfirm.Types as OnConfirm
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.API.Types (BecknCallbackReq)
import qualified Core.Context as Context
import qualified Core.OnConfirm as OnConfirm
import qualified Domain.Booking as DBooking
import Domain.PaymentTransaction (PaymentStatus (PENDING))
import qualified Domain.PaymentTransaction as DPaymentTransaction
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnConfirm.API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  BecknCallbackReq OnConfirm.OnConfirmMessage ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_confirm req" (encodeToText req)
  validateContext Context.ON_CONFIRM $ req.context
  case req.contents of
    Right msg -> handleOnConfirm (Id req.context.transaction_id) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  return Ack

handleOnConfirm :: EsqDBFlow m r => Id DBooking.Booking -> OnConfirm.OnConfirmMessage -> m ()
handleOnConfirm bookingId msg = do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  let updBooking =
        booking{status = DBooking.AWAITING_PAYMENT,
                bppOrderId = Just msg.order.id
               }
  paymentData <- buildPaymentData updBooking
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId updBooking
    PaymentTransactionDB.create paymentData

buildPaymentData :: MonadFlow m => DBooking.Booking -> m DPaymentTransaction.PaymentTransaction
buildPaymentData booking = do
  id <- generateGUID
  now <- getCurrentTime
  return
    DPaymentTransaction.PaymentTransaction
      { id = Id id,
        bookingId = booking.id,
        bknTxnId = getId booking.id,
        paymentGatewayTxnId = getId booking.id,
        fare = booking.fare,
        status = PENDING,
        paymentUrl = booking.bppUrl,
        updatedAt = now,
        createdAt = now
      }