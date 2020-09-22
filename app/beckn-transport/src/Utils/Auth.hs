module Utils.Auth where

import App.Types
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM401)
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import qualified Storage.Queries.Organization as OQ

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Organization
  verificationDescription =
    "Checks whether app/gateway is registered.\
    \If you don't have an API key, register the app/gateway."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction $ OQ.findOrgByApiKey >=> fromMaybeM401 "INVALID_API_KEY"
