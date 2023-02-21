{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module RiderPlatformClient.RiderApp
  ( callRiderApp,
    CustomerAPIs (..),
    AppBackendAPIs (..),
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified Dashboard.Common.Booking as Common
import qualified Dashboard.RiderPlatform.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as BAP
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data AppBackendAPIs = AppBackendAPIs
  { customers :: CustomerAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs
  }

data CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Integer -> Maybe Integer -> Euler.EulerClient Text,
    customerUpdate :: Id BAP.Person -> Text -> Euler.EulerClient Text,
    customerDelete :: Id Common.Customer -> Euler.EulerClient APISuccess
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Common.StuckBookingsCancelReq -> Euler.EulerClient Common.StuckBookingsCancelRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Common.MerchantUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceConfigUpdate :: Common.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Common.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Common.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Common.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId token = do
  let customers = CustomerAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  AppBackendAPIs {..}
  where
    customersClient
      :<|> bookingsClient
      :<|> merchantClient = clientWithMerchant (Proxy :: Proxy BAP.API') merchantId token

    customerList
      :<|> customerUpdate
      :<|> customerDelete = customersClient

    stuckBookingsCancel = bookingsClient

    merchantUpdate
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

callRiderApp ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (AppBackendAPIs -> b) ->
  c
callRiderApp merchantId = callServerAPI @_ @m @r APP_BACKEND (mkAppBackendAPIs merchantId) "callRiderApp"
