module Core.OnConfirm where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.OrderState
import Core.Payment
import Core.Provider
import Core.Quotation

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [OnConfirmItem],
    billing :: Billing,
    fulfillment :: OnConfirmFulfillment,
    quote :: OnConfirmQuotation,
    payment :: OnConfirmPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)

changePaymentState :: Status -> TrStatus -> Order -> Order
changePaymentState st trStatus ord =
  ord{payment =
        ord.payment
          { status = st,
            params = ord.payment.params {transaction_status = trStatus}
          }
     }

successfulPayment :: Order -> Order
successfulPayment = changePaymentState PAID Captured

failedTransaction :: TrStatus -> Order -> Order
failedTransaction = changePaymentState NOT_PAID

linkExpired :: Order -> Order
linkExpired = failedTransaction PaymentLinkExpired

paymentFailed :: Order -> Order
paymentFailed = failedTransaction Failed
