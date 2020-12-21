{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Status as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant ((:<|>) (..))

status :: Organization -> API.StatusReq -> FlowHandler AckResponse
status org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Status" $ do
        statusMessage <- mkStatusMessage
        AckResponse {} <-
          callClient "status" (req ^. #context) appUrl $
            onStatusAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_status"},
                  contents = Right statusMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    _ :<|> onStatusAPI = client API.onStatusAPI

mkStatusMessage :: Flow API.StatusResMessage
mkStatusMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ API.StatusResMessage example
