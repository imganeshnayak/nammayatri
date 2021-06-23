module Beckn.External.Exotel.Flow where

import Beckn.External.Exotel.Types
import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant
import Servant.Client

-- | Exotel API interface
type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ExotelRequest
    :> Post '[JSON] ExotelResponse

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy

defaultBaseUrl :: ExotelAccountSID -> BaseUrl
defaultBaseUrl sid =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.exotel.com",
      baseUrlPort = 443,
      baseUrlPath =
        T.unpack $
          "/v1/Accounts/"
            <> getExotelAccountSID sid
            <> "/Calls/connect.json"
    }

initiateCall ::
  HasFlowEnv m r '["exotelCfg" ::: Maybe ExotelCfg] =>
  T.Text ->
  T.Text ->
  m ()
initiateCall from to = do
  mbExotelCfg <- asks (.exotelCfg)
  fork forkDesc $
    case mbExotelCfg of
      Just ExotelCfg {..} -> do
        let apiKey_ = ExotelApiKey apiKey
            apiToken_ = ExotelApiToken apiToken
            sid_ = ExotelAccountSID sid
            exoRequest = ExotelRequest from to callerId
            authData =
              BasicAuthData
                (DT.encodeUtf8 $ getExotelApiKey apiKey_)
                (DT.encodeUtf8 $ getExotelApiToken apiToken_)
        res <-
          callAPI
            (defaultBaseUrl sid_)
            ( callExotel authData exoRequest
            )
            "initiateCall"
        logTagInfo exotel $ case res of
          Right _ -> "call initiated from " <> from <> " to " <> to
          Left x -> "error: " <> show x
      _ -> logTagError exotel "exotel ENV vars are not properly set"
  where
    callExotel authData exoRequest = void $ ET.client exotelConnectAPI authData exoRequest
    forkDesc = "Exotel initiate call forked flow " <> from <> " " <> to
    exotel = T.pack "Exotel"
