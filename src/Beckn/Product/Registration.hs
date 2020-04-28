module Beckn.Product.Registration where

import qualified Beckn.Data.Accessor          as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.Registration
import           Beckn.Types.App
import           Data.Aeson
import           EulerHS.Prelude

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin loginRes = undefined

login :: Text -> LoginReq -> FlowHandler Value
login tokenId req = do
  case req ^. Accessor.action of
    VERIFY -> undefined
    RESEND -> undefined
