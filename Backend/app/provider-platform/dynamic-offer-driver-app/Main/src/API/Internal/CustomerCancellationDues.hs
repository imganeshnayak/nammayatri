module API.Internal.CustomerCancellationDues
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.CustomerCancellationDues as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> "dispute"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.CancellationDuesReq
    :> Post '[JSON] APISuccess
    :<|> Capture "merchantId" (Id Merchant)
      :> "getCancellationDuesDetails"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.CancellationDuesReq
      :> Get '[JSON] Domain.CancellationDuesDetailsRes
    :<|> Capture "merchantId" (Id Merchant)
      :> "customerCancellationDuesSync"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.CustomerCancellationDuesSyncReq
      :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  disputeCancellationDues
    :<|> getCancellationDuesDetails
    :<|> customerCancellationDuesSync

disputeCancellationDues :: Id Merchant -> Maybe Text -> Domain.CancellationDuesReq -> FlowHandler APISuccess
disputeCancellationDues merchantId apiKey = withFlowHandlerAPI . Domain.disputeCancellationDues merchantId apiKey

getCancellationDuesDetails :: Id Merchant -> Maybe Text -> Domain.CancellationDuesReq -> FlowHandler Domain.CancellationDuesDetailsRes
getCancellationDuesDetails merchantId apiKey = withFlowHandlerAPI . Domain.getCancellationDuesDetails merchantId apiKey

customerCancellationDuesSync :: Id Merchant -> Maybe Text -> Domain.CustomerCancellationDuesSyncReq -> FlowHandler APISuccess
customerCancellationDuesSync merchantId apiKey = withFlowHandlerAPI . Domain.customerCancellationDuesSync merchantId apiKey
