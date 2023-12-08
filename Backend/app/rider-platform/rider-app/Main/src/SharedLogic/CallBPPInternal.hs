module SharedLogic.CallBPPInternal where

import Domain.Types.FeedbackForm
import EulerHS.Types (EulerClient, client)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common hiding (Error)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Text,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "referee"
    :> Header "token" Text
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] APISuccess

linkRefereeClient :: Text -> Maybe Text -> RefereeLinkInfoReq -> EulerClient APISuccess
linkRefereeClient = client likeRefereeApi

likeRefereeApi :: Proxy LinkRefereeAPI
likeRefereeApi = Proxy

linkReferee ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  m APISuccess
linkReferee apiKey internalUrl merchantId referralCode phoneNumber countryCode = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (linkRefereeClient merchantId (Just apiKey) (RefereeLinkInfoReq referralCode phoneNumber countryCode)) "LinkReferee" likeRefereeApi

type FeedbackFormAPI =
  "internal"
    :> "beckn"
    :> "booking"
    :> ( "feedback"
           :> ReqBody '[JSON] FeedbackFormReq
           :> Post '[JSON] APISuccess
       )

feedbackFormClient :: FeedbackFormReq -> EulerClient APISuccess
feedbackFormClient = client (Proxy @FeedbackFormAPI)

feedbackFormApi :: Proxy FeedbackFormAPI
feedbackFormApi = Proxy

feedbackForm ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  FeedbackFormReq ->
  m APISuccess
feedbackForm internalUrl request = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (feedbackFormClient request) "FeedbackForm" feedbackFormApi

data CancellationDuesReq = CancellationDuesReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type DisputeCancellationDuesAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "dispute"
    :> Header "token" Text
    :> ReqBody '[JSON] CancellationDuesReq
    :> Post '[JSON] APISuccess

disputeCancellationDuesClient :: Text -> Maybe Text -> CancellationDuesReq -> EulerClient APISuccess
disputeCancellationDuesClient = client disputeCancellationDuesApi

disputeCancellationDuesApi :: Proxy DisputeCancellationDuesAPI
disputeCancellationDuesApi = Proxy

disputeCancellationDues ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m APISuccess
disputeCancellationDues apiKey internalUrl merchantId phoneNumber countryCode = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (disputeCancellationDuesClient merchantId (Just apiKey) (CancellationDuesReq phoneNumber countryCode)) "DisputeCancellationDues" disputeCancellationDuesApi

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { customerCancellationDues :: Money,
    disputeChancesUsed :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type GetCancellationDuesDetailsAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "getCancellationDuesDetails"
    :> Header "token" Text
    :> ReqBody '[JSON] CancellationDuesReq
    :> Get '[JSON] CancellationDuesDetailsRes

getCancellationDuesDetailsClient :: Text -> Maybe Text -> CancellationDuesReq -> EulerClient CancellationDuesDetailsRes
getCancellationDuesDetailsClient = client getCancellationDuesDetailsApi

getCancellationDuesDetailsApi :: Proxy GetCancellationDuesDetailsAPI
getCancellationDuesDetailsApi = Proxy

getCancellationDuesDetails ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m CancellationDuesDetailsRes
getCancellationDuesDetails apiKey internalUrl merchantId phoneNumber countryCode = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (getCancellationDuesDetailsClient merchantId (Just apiKey) (CancellationDuesReq phoneNumber countryCode)) "GetCancellationDuesDetails" getCancellationDuesDetailsApi

type CustomerCancellationDuesSyncAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "customerCancellationDuesSync"
    :> Header "token" Text
    :> ReqBody '[JSON] CustomerCancellationDuesSyncReq
    :> Post '[JSON] APISuccess

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    cancellationCharges :: Maybe Money,
    disputeChancesUsed :: Maybe Int,
    paymentMadeToDriver :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

customerCancellationDuesSyncClient :: Text -> Maybe Text -> CustomerCancellationDuesSyncReq -> EulerClient APISuccess
customerCancellationDuesSyncClient = client customerCancellationDuesSyncApi

customerCancellationDuesSyncApi :: Proxy CustomerCancellationDuesSyncAPI
customerCancellationDuesSyncApi = Proxy

customerCancellationDuesSync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe Money ->
  Maybe Int ->
  Bool ->
  m APISuccess
customerCancellationDuesSync apiKey internalUrl merchantId phoneNumber countryCode cancellationCharges disputeChancesUsed paymentMadeToDriver = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (customerCancellationDuesSyncClient merchantId (Just apiKey) (CustomerCancellationDuesSyncReq phoneNumber countryCode cancellationCharges disputeChancesUsed paymentMadeToDriver)) "CustomerCancellationDuesSync" customerCancellationDuesSyncApi
