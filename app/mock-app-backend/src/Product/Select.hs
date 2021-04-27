{-# LANGUAGE OverloadedLabels #-}

module Product.Select where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack (AckResponse (..), Status (..), ack)
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Data.Aeson (encode)
import EulerHS.Prelude
import EulerHS.Types (client)
import Types.Error

selectCb :: Organization -> API.OnSelectReq -> FlowHandler AckResponse
selectCb _org req = withFlowHandlerBecknAPI $ do
  let resp = AckResponse (req ^. #context) (ack ACK) Nothing
  ctx <- updateCaller $ req ^. #context
  logTagDebug "mock_app_backend" $
    "select_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  case req ^. #contents of
    Right msg -> do
      quote <- (msg ^. #order . #_quotation) & fromMaybeM (InvalidRequest "You should pass quotation.")
      let quoteId = quote ^. #_id
      initReq <- buildInitReq ctx quoteId
      case req ^. #context . #_bpp_uri of
        Nothing -> logTagError "mock-app-backend" "Bad ac_id"
        Just url ->
          void $
            callClient' (Just HttpSig.signatureAuthManagerKey) "init" (req ^. #context) url $
              client API.initAPI initReq
    Left err -> logTagDebug "mock_app_backend" $ "select_cb error: " <> show err
  return resp
