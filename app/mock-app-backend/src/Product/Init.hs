{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)

initCb :: Organization -> OnInitReq -> FlowHandler AckResponse
initCb org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  EL.logDebug @Text "mock_app_backend" $ "init_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  confirmReq <- buildConfirmReq ctx
  apiKey <- org ^. #_apiKey & fromMaybeM500 "API_KEY_NOT_CONFIGURED"
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
    Just url ->
      void $
        callClient "confirm" url $
          client confirmAPI apiKey confirmReq
  return resp
