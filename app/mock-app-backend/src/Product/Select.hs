{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant ((:<|>) (..))

selectCb :: Organization -> API.OnSelectReq -> FlowHandler AckResponse
selectCb org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  EL.logDebug @Text "mock_app_backend" $
    "select_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  case req ^. #contents of
    Right msg -> do
      quote <- (msg ^. #order . #_quotation) & fromMaybeM400 "INVALID_QUOTE"
      let quoteId = quote ^. #_id
      initReq <- buildInitReq ctx quoteId
      cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "API_KEY_NOT_CONFIGURED"
      case req ^. #context . #_bpp_uri of
        Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
        Just url ->
          void $
            callClient "init" (req ^. #context) url $
              initAPI cbApiKey initReq
    Left err -> EL.logDebug @Text "mock_app_backend" $ "select_cb error: " <> show err
  return resp
  where
    _ :<|> initAPI = client API.initAPI
