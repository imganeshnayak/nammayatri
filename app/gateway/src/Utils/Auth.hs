module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM)
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import qualified Storage.Queries.App as BA
import qualified Storage.Queries.Provider as BP
import Types.Error

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Org.Organization
  verificationDescription =
    "Checks whether app/provider is registered.\
    \If you don't have an API key, register with the gateway."

verifyAPIKeyAction :: VerificationAction VerifyAPIKey AppEnv
verifyAPIKeyAction = VerificationAction $ \apiKey -> do
  app <- BA.lookupKey Org.APP apiKey
  provider <- BP.lookupKey Org.PROVIDER apiKey
  app <|> provider
    & fromMaybeM (InvalidRequest "Invalid api key")
