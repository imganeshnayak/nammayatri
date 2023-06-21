{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Payment.PaymentTransaction where

import qualified Domain.Types.Merchant as DM
import Domain.Types.Payment.PaymentOrder as DOrder
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data PaymentTransaction = PaymentTransaction
  { id :: Id PaymentTransaction,
    txnUUID :: Text,
    paymentMethodType :: Text,
    paymentMethod :: Text,
    respMessage :: Maybe Text,
    respCode :: Maybe Text,
    gatewayReferenceId :: Maybe Text,
    orderId :: Id DOrder.PaymentOrder,
    merchantId :: Id DM.Merchant,
    amount :: HighPrecMoney,
    currency :: Payment.Currency,
    dateCreated :: Maybe UTCTime,
    statusId :: Int,
    status :: Payment.TransactionStatus,
    juspayResponse :: Maybe Text, -- webhook resp dump
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)
