{-# LANGUAGE OverloadedLabels #-}

module Product.Status where

import App.Types
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.FMD.API.Status
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import Data.Aeson (encode)
import EulerHS.Prelude

statusCb :: Organization -> OnStatusReq -> FlowHandler AckResponse
statusCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  logDebug "mock_app_backend" $ "status_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case req ^. #context . #_bpp_uri of
    Nothing -> logError "mock-app-backend" "Bad ac_id"
    Just _ -> logDebug "mock-app-backend" "Status delivered Successfully"
  return resp
