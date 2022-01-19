module API.Types where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Core.Confirm
import Core.Init
import Core.Search
import Core.Status
import Servant

type HealthCheckAPI = "health" :> Get '[JSON] Text

type SearchAPI =
  "search" :> ReqBody '[JSON] (BecknReq SearchMessage) :> Post '[JSON] AckResponse

type InitAPI =
  "init" :> ReqBody '[JSON] (BecknReq InitMessage) :> Post '[JSON] AckResponse

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] AckResponse

type StatusAPI =
  "status" :> ReqBody '[JSON] (BecknReq StatusMessage) :> Post '[JSON] AckResponse

type TotalAPI = HealthCheckAPI :<|> SearchAPI :<|> InitAPI :<|> ConfirmAPI :<|> StatusAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy
