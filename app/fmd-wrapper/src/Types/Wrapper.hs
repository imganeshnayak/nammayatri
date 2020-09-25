module Types.Wrapper where

import Beckn.Types.App
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.Order
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import External.Dunzo.Types (ClientId, ClientSecret)

-- BAP with Dunzo account will have these details
-- in `organization.info`
data DzBAConfig = DzBAConfig
  { bapId :: Text,
    dzClientId :: ClientId,
    dzClientSecret :: ClientSecret
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DunzoConfig = DunzoConfig
  { dzUrl :: BaseUrl,
    dzTokenUrl :: BaseUrl,
    dzBPId :: Text,
    dzBPNwAddress :: BaseUrl,
    payee :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)

data OrderDetails = OrderDetails
  { order :: Order,
    quote :: Quotation
  }
  deriving (Show, Generic, ToJSON, FromJSON)
