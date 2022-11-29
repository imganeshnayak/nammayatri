module API.UI.Registration (module Reexport, API, handler) where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Registration as Reexport
  ( AuthReq (..),
    AuthRes (..),
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    ResendAuthRes,
  )
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import Environment
import EulerHS.Prelude hiding (id)
import Servant hiding (throwError)
import Tools.Auth (TokenAuth)

type API =
  "auth"
    :> ( ReqBody '[JSON] AuthReq
           :> Header "x-bundle-version" Text
           :> Header "x-client-version" Text
           :> Post '[JSON] AuthRes
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] AuthVerifyReq
             :> Post '[JSON] AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> "resend"
             :> Post '[JSON] ResendAuthRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  auth
    :<|> verify
    :<|> resend
    :<|> logout

auth :: AuthReq -> Maybe Text -> Maybe Text -> FlowHandler AuthRes
auth req mbBundleVersion = withFlowHandlerAPI . DReg.auth req mbBundleVersion

verify :: Id SR.RegistrationToken -> AuthVerifyReq -> FlowHandler AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DReg.verify tokenId

resend :: Id SR.RegistrationToken -> FlowHandler ResendAuthRes
resend = withFlowHandlerAPI . DReg.resend

logout :: Id SP.Person -> FlowHandler APISuccess
logout = withFlowHandlerAPI . DReg.logout
