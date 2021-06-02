module Beckn.Types.Core.Migration.Payment
  ( Payment (..),
    PaymentType (..),
    TLMethod (..),
    Params (..),
  )
where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON
import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.HashMap.Strict (delete)
import EulerHS.Prelude hiding (State, (.=))
import Servant.Client (BaseUrl)

data Payment = Payment
  { uri :: Maybe BaseUrl,
    tl_method :: Maybe TLMethod,
    params :: Maybe Params,
    payee :: Maybe Payee,
    _type :: Maybe PaymentType,
    status :: Maybe Status,
    time :: Maybe Time
  }
  deriving (Generic, Show)

data TLMethod = HttpGet | HttpPost
  deriving (Show)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

data Params = Params
  { transaction_id :: Maybe Text,
    amount :: Maybe DecimalValue,
    additional :: HashMap Text Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON Params where
  parseJSON = withObject "Params" $ \o ->
    Params
      <$> o .: "transaction_id"
      <*> o .: "amount"
      <*> mapM f (additional o)
    where
      f (String val) = pure val
      f e = typeMismatch "additional property of Params" e
      additional = delete "transaction_id" . delete "amount"

instance ToJSON Params where
  toJSON Params {..} = uniteObjects [object knownParams, Object (String <$> additional)]
    where
      knownParams =
        [ "transaction_id" .= transaction_id,
          "amount" .= amount
        ]

data Payee = PersonPayee Person | VPA Text | BankAccPayment BankAccount
  deriving (Generic, Eq, Show)

instance FromJSON Payee where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing payeeConstructorMapping

instance ToJSON Payee where
  toJSON = genericToJSON $ objectWithSingleFieldParsing payeeConstructorMapping

payeeConstructorMapping :: String -> String
payeeConstructorMapping = \case
  "PersonPayee" -> "person"
  "VPA" -> "vpa"
  "BankAccPayment" -> "bank_account"
  err -> error "Unexpected constructor name \"" <> err <> "\" in function payeeConstructorMapping"

data BankAccount = BankAccount
  { ifsc_code :: Maybe Text,
    account_number :: Maybe Text,
    account_holder_name :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON BankAccount where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BankAccount where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens
