{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Merchant
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (Meters, MonadFlow, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> MerchantCommonConfigAPI
           :<|> MerchantCommonConfigUpdateAPI
           :<|> DriverPoolConfigAPI
           :<|> DriverPoolConfigUpdateAPI
           :<|> DriverPoolConfigCreateAPI
           :<|> DriverIntelligentPoolConfigAPI
           :<|> DriverIntelligentPoolConfigUpdateAPI
           :<|> OnboardingDocumentConfigAPI
           :<|> OnboardingDocumentConfigUpdateAPI
           :<|> OnboardingDocumentConfigCreateAPI
           :<|> ServiceUsageConfigAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
           :<|> VerificationServiceConfigUpdateAPI
           :<|> CreateFPDriverExtraFee
           :<|> UpdateFPDriverExtraFee
       )

type MerchantUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MERCHANT_UPDATE
    :> Common.MerchantUpdateAPI

type MerchantCommonConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MERCHANT_COMMON_CONFIG
    :> Common.MerchantCommonConfigAPI

type MerchantCommonConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MERCHANT_COMMON_CONFIG_UPDATE
    :> Common.MerchantCommonConfigUpdateAPI

type DriverPoolConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'DRIVER_POOL_CONFIG
    :> Common.DriverPoolConfigAPI

type DriverPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'DRIVER_POOL_CONFIG_UPDATE
    :> Common.DriverPoolConfigUpdateAPI

type DriverPoolConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'DRIVER_POOL_CONFIG_CREATE
    :> Common.DriverPoolConfigCreateAPI

type DriverIntelligentPoolConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'DRIVER_INTELLIGENT_POOL_CONFIG
    :> Common.DriverIntelligentPoolConfigAPI

type DriverIntelligentPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE
    :> Common.DriverIntelligentPoolConfigUpdateAPI

type OnboardingDocumentConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG
    :> Common.OnboardingDocumentConfigAPI

type OnboardingDocumentConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG_UPDATE
    :> Common.OnboardingDocumentConfigUpdateAPI

type OnboardingDocumentConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG_CREATE
    :> Common.OnboardingDocumentConfigCreateAPI

type ServiceUsageConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'SERVICE_USAGE_CONFIG
    :> Common.ServiceUsageConfigAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MAPS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'SMS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.SmsServiceUsageConfigUpdateAPI

type VerificationServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'VERIFICATION_SERVICE_CONFIG_UPDATE
    :> Common.VerificationServiceConfigUpdateAPI

type CreateFPDriverExtraFee =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'CREATE_FP_DRIVER_EXTRA_FEE
    :> Common.CreateFPDriverExtraFee

type UpdateFPDriverExtraFee =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'UPDATE_FP_DRIVER_EXTRA_FEE
    :> Common.UpdateFPDriverExtraFee

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> merchantCommonConfig merchantId city
    :<|> merchantCommonConfigUpdate merchantId city
    :<|> driverPoolConfig merchantId city
    :<|> driverPoolConfigUpdate merchantId city
    :<|> driverPoolConfigCreate merchantId city
    :<|> driverIntelligentPoolConfig merchantId city
    :<|> driverIntelligentPoolConfigUpdate merchantId city
    :<|> onboardingDocumentConfig merchantId city
    :<|> onboardingDocumentConfigUpdate merchantId city
    :<|> onboardingDocumentConfigCreate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> verificationServiceConfigUpdate merchantId city
    :<|> createFPDriverExtraFee merchantId city
    :<|> updateFPDriverExtraFee merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MerchantEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MerchantAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.merchantUpdate) req

merchantCommonConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.MerchantCommonConfigRes
merchantCommonConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId (.merchant.merchantCommonConfig)

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantCommonConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.merchantCommonConfigUpdate) req

driverPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Meters ->
  FlowHandler Common.DriverPoolConfigRes
driverPoolConfig merchantShortId opCity apiTokenInfo tripDistance = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId (.merchant.driverPoolConfig) tripDistance

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId opCity apiTokenInfo tripDistance req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverPoolConfigUpdate) tripDistance req

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId opCity apiTokenInfo tripDistance req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverPoolConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverPoolConfigCreate) tripDistance req

driverIntelligentPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId (.merchant.driverIntelligentPoolConfig)

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverIntelligentPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverIntelligentPoolConfigUpdate) req

onboardingDocumentConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Common.DocumentType ->
  FlowHandler Common.OnboardingDocumentConfigRes
onboardingDocumentConfig merchantShortId opCity apiTokenInfo documentType = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId (.merchant.onboardingDocumentConfig) documentType

onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigUpdate merchantShortId opCity apiTokenInfo documentType req = withFlowHandlerAPI $ do
  -- runRequestValidation Common.validateOnboardingDocumentConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.OnboardingDocumentConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.onboardingDocumentConfigUpdate) documentType req

onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigCreate merchantShortId opCity apiTokenInfo documentType req = withFlowHandlerAPI $ do
  -- runRequestValidation Common.validateOnboardingDocumentConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.OnboardingDocumentConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.onboardingDocumentConfigCreate) documentType req

serviceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId (.merchant.serviceUsageConfig)

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.mapsServiceUsageConfigUpdate) req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.smsServiceConfigUpdate) req

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.smsServiceUsageConfigUpdate) req

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.VerificationServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.verificationServiceConfigUpdate) req

createFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
createFPDriverExtraFee merchantShortId opCity apiTokenInfo farePolicyId startDistance req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.CreateFPDriverExtraFeeEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPP checkedMerchantId (.merchant.createFPDriverExtraFee) farePolicyId startDistance req

updateFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
updateFPDriverExtraFee merchantShortId opCity apiTokenInfo farePolicyId startDistance req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateFPDriverExtraFeeEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPP checkedMerchantId (.merchant.updateFPDriverExtraFee) farePolicyId startDistance req
