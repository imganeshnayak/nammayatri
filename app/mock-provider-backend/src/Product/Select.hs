{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

select :: () -> SelectReq -> FlowHandlerR r AckResponse
select _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
      context =
        (req ^. #context)
          { _ac_id = fromString <$> bppNwAddr -- update caller id
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad ac_id"
    Just appUrl ->
      forkAsync "Select" $ do
        onSelectMessage <- mkQuote
        AckResponse {} <-
          callClient "select" appUrl $
            client
              onSelectAPI
              "test-provider-2-key"
              CallbackReq
                { context = context {_action = "on_select"},
                  contents = Right onSelectMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkQuote :: FlowR r OnSelectMessage
mkQuote = do
  L.runIO $ threadDelay 0.5e6
  return $ OnSelectMessage example example
