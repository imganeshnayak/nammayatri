{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Server where

import Beckn.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Beckn.Utils.Common hiding (throwError)
import Data.UUID.V4 (nextRandom)
import EulerHS.Language
import EulerHS.Prelude
import Servant

class HasEnvEntry r (context :: [Type]) | context -> r where
  getEnvEntry :: Context context -> EnvR r

instance {-# OVERLAPPABLE #-} HasEnvEntry r xs => HasEnvEntry r (notIt ': xs) where
  getEnvEntry (_ :. xs) = getEnvEntry xs

instance {-# OVERLAPPING #-} HasEnvEntry r (EnvR r ': xs) where
  getEnvEntry (x :. _) = x

run ::
  forall a r ctx.
  ( HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer a (EnvR r ': ctx)
  ) =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  Context ctx ->
  EnvR r ->
  Application
run apis server ctx env =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(EnvR r ': ctx)) f server
  where
    f :: FlowHandlerR r m -> Handler m
    f r = do
      eResult <- liftIO $ do
        uuid <- nextRandom
        let modEnv = modifiedEnvR $ show uuid
        try $ runReaderT r modEnv
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
    modifiedEnvR uuid =
      env {flowRuntime = updateLoggerContext (appendLogContext uuid) $ flowRuntime env}
