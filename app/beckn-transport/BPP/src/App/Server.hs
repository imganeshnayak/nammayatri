module App.Server where

import API (transporterAPI, transporterServer)
import Beckn.Tools.Metrics.Init
import Beckn.Types.Flow
import Beckn.Utils.App
import qualified Beckn.Utils.Servant.Server as BU
import Core.Beckn (logBecknRequest)
import Environment
import EulerHS.Prelude
import Servant
import Tools.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run transporterAPI transporterServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & logBecknRequest modifiedEnv.appEnv
    & addServantInfo transporterAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. EmptyContext
