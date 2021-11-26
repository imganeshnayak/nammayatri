module Beckn.Utils.Context where

import Beckn.Types.App
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import qualified Beckn.Types.Core.Migration1.Common.Context as Mig1
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

buildMobilityContext ::
  (MonadTime m, MonadGuid m) =>
  Text ->
  Text ->
  Maybe BaseUrl ->
  Maybe BaseUrl ->
  m Context
buildMobilityContext action txnId bapUri bppUri = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    Context
      { domain = MOBILITY,
        country = Just "IND",
        city = Just "Kochi",
        action = action,
        core_version = Just "0.8.2",
        domain_version = Just "0.8.2",
        bap_uri = bapUri,
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = currTime,
        ttl = Nothing
      }

updateContext :: (MonadTime m, MonadGuid m) => Text -> Context -> m Context
updateContext action context = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    context & #timestamp .~ currTime
      & #message_id .~ msgId
      & #action .~ action

buildMobilityContext1 ::
  (MonadTime m, MonadGuid m) =>
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Mig1.Context
buildMobilityContext1 txnId bapUri bppUri = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    Mig1.Context
      { domain = Mig1.MOBILITY,
        core_version = "0.9",
        bap_id = Just "",
        bap_uri = bapUri,
        bpp_id = Just "",
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = currTime
      }

updateContext1 :: (MonadTime m, MonadGuid m) => Mig1.Context -> m Mig1.Context
updateContext1 context = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    context & #timestamp .~ currTime
      & #message_id .~ msgId
