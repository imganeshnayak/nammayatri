module Types.DBSync.Update where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
import Euler.DB.Storage.Types
  ( AgencyT,
    AuthMappingT,
    -- TODO: Export below tables from euler-db
    -- , BankInfoT
    -- , ClientConfigurationT
    -- , ConfigurationsT
    -- , EncryptionKeysT
    -- , GatewayTxnStatusMapT
    -- , JuspayErrorMapT
    -- , JuspayIssuerNameMappingT
    -- , MerchantPriorityLogicT
    -- , MerchantCustomerCofDetailsT
    -- , UserAclT
    AuthenticationAccountT,
    BankAccountT,
    CardBrandRoutesT,
    CardInfoT,
    ChargebackT,
    CofDetailsT,
    CustomerAccountT,
    CustomerT,
    DeviceBindingT,
    EmiPlanT,
    EnrolledPanT,
    EntityMapT,
    ExternalMerchantCustomerT,
    FeatureT,
    FormInputT,
    GatewayBankEmiSupportT,
    GatewayCardInfoT,
    GatewayHealthT,
    GatewayOutageT,
    GatewayPaymentMethodT,
    GatewayStatusMapT,
    GatewayTxnDataT,
    HdfcHashedNumT,
    IngressRuleT,
    InstallmentRefundT,
    InstallmentT,
    IsinRoutesT,
    IssuerRoutesT,
    JuspayBankCodeT,
    JuspayEventT,
    LockerAccountT,
    LockerTokenRequestorT,
    MandateT,
    MerchantAccountT,
    MerchantGatewayAccountSubInfoT,
    MerchantGatewayAccountT,
    MerchantGatewayCardInfoT,
    MerchantGatewayPaymentMethodT,
    MerchantIframePreferencesT,
    MerchantKeyT,
    MerchantLockerAccountT,
    MerchantProviderDetailsT,
    MerchantRiskSettingsT,
    MetadataT,
    NetworkCardFingerprintT,
    NotificationT,
    OfferBenefitInfoT,
    OfferRedemptionT,
    OffersT,
    OrderAddressT,
    OrderBasketT,
    OrderMetadataV2T,
    OrderReferenceT,
    PaymentFormT,
    PaymentGatewayResponseT,
    PaymentGatewayResponseV1T,
    PaymentLinksT,
    PaymentMethodT,
    ProcessTrackerT,
    PromotionT,
    ProviderT,
    RefundT,
    ResellerAccountT,
    RiskManagementAccountT,
    RoleT,
    RuleT,
    SavedPaymentMethodT,
    SecondFactorResponseT,
    SecondFactorT,
    StoredCardT,
    TempCardT,
    TokenBinInfoT,
    TokenCustomerT,
    TokenRequestorT,
    TokenT,
    TxnCardInfoT,
    TxnDetailT,
    TxnIntentDetailT,
    TxnOfferDetailT,
    TxnOfferInfoT,
    TxnOfferT,
    TxnRiskCheckT,
    UnifiedGatewayResponseT,
    UserRoleT,
    UserT,
    WalletAccountT,
    WalletTopUpTxnT,
  )
import EulerHS.Prelude
import Sequelize
import Utils.Parse

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.
data UpdateModel
  = TxnOfferInfoUpdate
  | JuspayEventUpdate
  | HdfcHashedNumUpdate
  | MerchantGatewayAccountUpdate
  | RuleUpdate
  | MerchantGatewayAccountSubInfoUpdate
  | OrderReferenceUpdate
  | MandateUpdate
  | GatewayTxnDataUpdate
  | MerchantGatewayCardInfoUpdate
  | TxnRiskCheckUpdate
  | PaymentMethodUpdate
  | OfferBenefitInfoUpdate
  | MerchantIframePreferencesUpdate
  | ResellerAccountUpdate
  | UnifiedGatewayResponseUpdate
  | WalletTopUpTxnUpdate
  | IsinRoutesUpdate
  | TxnCardInfoUpdate
  | OrderAddressUpdate
  | LockerAccountUpdate
  | JuspayBankCodeUpdate
  | IssuerRoutesUpdate
  | RefundUpdate
  | SecondFactorUpdate
  | GatewayOutageUpdate
  | TxnDetailUpdate
  | EmiPlanUpdate
  | MerchantKeyUpdate
  | NetworkCardFingerprintUpdate
  | TokenRequestorUpdate
  | TxnOfferDetailUpdate
  | SecondFactorResponseUpdate
  | OfferRedemptionUpdate
  | CardBrandRoutesUpdate
  | CustomerUpdate
  | MerchantAccountUpdate
  | ExternalMerchantCustomerUpdate
  | TokenBinInfoUpdate
  | MerchantGatewayPaymentMethodUpdate
  | PromotionUpdate
  | LockerTokenRequestorUpdate
  | BankAccountUpdate
  | AgencyUpdate
  | ProviderUpdate
  | GatewayCardInfoUpdate
  | PaymentGatewayResponseUpdate
  | MetadataUpdate
  | ChargebackUpdate
  | WalletAccountUpdate
  | GatewayStatusMapUpdate
  | TokenUpdate
  | MerchantLockerAccountUpdate
  | TempCardUpdate
  | MerchantRiskSettingsUpdate
  | UserUpdate
  | CofDetailsUpdate
  | OrderMetadataV2Update
  | StoredCardUpdate
  | TokenCustomerUpdate
  | EnrolledPanUpdate
  | OffersUpdate
  | RoleUpdate
  | FeatureUpdate
  | GatewayBankEmiSupportUpdate
  | AuthenticationAccountUpdate
  | PaymentGatewayResponseV1Update
  | SavedPaymentMethodUpdate
  | MerchantProviderDetailsUpdate
  | TxnOfferUpdate
  | GatewayHealthUpdate
  | RiskManagementAccountUpdate
  | CardInfoUpdate
  | DeviceBindingUpdate
  | NotificationUpdate
  | OrderBasketUpdate
  | GatewayPaymentMethodUpdate
  | ProcessTrackerUpdate
  | PaymentLinksUpdate
  | CustomerAccountUpdate
  | EntityMapUpdate
  | FormInputUpdate
  | IngressRuleUpdate
  | InstallmentUpdate
  | InstallmentRefundUpdate
  | PaymentFormUpdate
  | UserRoleUpdate
  | TxnIntentDetailUpdate
  | AuthMappingUpdate
  deriving (Generic, Show)

getTagUpdate :: UpdateModel -> Text
getTagUpdate TxnOfferInfoUpdate = "TxnOfferInfoOptions"
getTagUpdate JuspayEventUpdate = "JuspayEventOptions"
getTagUpdate HdfcHashedNumUpdate = "HdfcHashedNumOptions"
getTagUpdate MerchantGatewayAccountUpdate = "MerchantGatewayAccountOptions"
getTagUpdate RuleUpdate = "RuleOptions"
getTagUpdate MerchantGatewayAccountSubInfoUpdate = "MerchantGatewayAccountSubInfoOptions"
getTagUpdate OrderReferenceUpdate = "OrderReferenceOptions"
getTagUpdate MandateUpdate = "MandateOptions"
getTagUpdate GatewayTxnDataUpdate = "GatewayTxnDataOptions"
getTagUpdate MerchantGatewayCardInfoUpdate = "MerchantGatewayCardInfoOptions"
getTagUpdate TxnRiskCheckUpdate = "TxnRiskCheckOptions"
getTagUpdate PaymentMethodUpdate = "PaymentMethodOptions"
getTagUpdate OfferBenefitInfoUpdate = "OfferBenefitInfoOptions"
getTagUpdate MerchantIframePreferencesUpdate = "MerchantIframePreferencesOptions"
getTagUpdate ResellerAccountUpdate = "ResellerAccountOptions"
getTagUpdate UnifiedGatewayResponseUpdate = "UnifiedGatewayResponseOptions"
getTagUpdate WalletTopUpTxnUpdate = "WalletTopUpTxnOptions"
getTagUpdate IsinRoutesUpdate = "IsinRoutesOptions"
getTagUpdate TxnCardInfoUpdate = "TxnCardInfoOptions"
getTagUpdate OrderAddressUpdate = "OrderAddressOptions"
getTagUpdate LockerAccountUpdate = "LockerAccountOptions"
getTagUpdate JuspayBankCodeUpdate = "JuspayBankCodeOptions"
getTagUpdate IssuerRoutesUpdate = "IssuerRoutesOptions"
getTagUpdate RefundUpdate = "RefundOptions"
getTagUpdate SecondFactorUpdate = "SecondFactorOptions"
getTagUpdate GatewayOutageUpdate = "GatewayOutageOptions"
getTagUpdate TxnDetailUpdate = "TxnDetailOptions"
getTagUpdate EmiPlanUpdate = "EmiPlanOptions"
getTagUpdate MerchantKeyUpdate = "MerchantKeyOptions"
getTagUpdate NetworkCardFingerprintUpdate = "NetworkCardFingerprintOptions"
getTagUpdate TokenRequestorUpdate = "TokenRequestorOptions"
getTagUpdate TxnOfferDetailUpdate = "TxnOfferDetailOptions"
getTagUpdate SecondFactorResponseUpdate = "SecondFactorResponseOptions"
getTagUpdate OfferRedemptionUpdate = "OfferRedemptionOptions"
getTagUpdate CardBrandRoutesUpdate = "CardBrandRoutesOptions"
getTagUpdate CustomerUpdate = "CustomerOptions"
getTagUpdate MerchantAccountUpdate = "MerchantAccountOptions"
getTagUpdate ExternalMerchantCustomerUpdate = "ExternalMerchantCustomerOptions"
getTagUpdate TokenBinInfoUpdate = "TokenBinInfoOptions"
getTagUpdate MerchantGatewayPaymentMethodUpdate = "MerchantGatewayPaymentMethodOptions"
getTagUpdate PromotionUpdate = "PromotionOptions"
getTagUpdate LockerTokenRequestorUpdate = "LockerTokenRequestorOptions"
getTagUpdate BankAccountUpdate = "BankAccountOptions"
getTagUpdate AgencyUpdate = "AgencyOptions"
getTagUpdate ProviderUpdate = "ProviderOptions"
getTagUpdate GatewayCardInfoUpdate = "GatewayCardInfoOptions"
getTagUpdate PaymentGatewayResponseUpdate = "PaymentGatewayResponseOptions"
getTagUpdate MetadataUpdate = "MetadataOptions"
getTagUpdate ChargebackUpdate = "ChargebackOptions"
getTagUpdate WalletAccountUpdate = "WalletAccountOptions"
getTagUpdate GatewayStatusMapUpdate = "GatewayStatusMapOptions"
getTagUpdate TokenUpdate = "TokenOptions"
getTagUpdate MerchantLockerAccountUpdate = "MerchantLockerAccountOptions"
getTagUpdate TempCardUpdate = "TempCardOptions"
getTagUpdate MerchantRiskSettingsUpdate = "MerchantRiskSettingsOptions"
getTagUpdate UserUpdate = "UserOptions"
getTagUpdate CofDetailsUpdate = "CofDetailsOptions"
getTagUpdate OrderMetadataV2Update = "OrderMetadataV2Options"
getTagUpdate StoredCardUpdate = "StoredCardOptions"
getTagUpdate TokenCustomerUpdate = "TokenCustomerOptions"
getTagUpdate EnrolledPanUpdate = "EnrolledPanOptions"
getTagUpdate OffersUpdate = "OffersOptions"
getTagUpdate RoleUpdate = "RoleOptions"
getTagUpdate FeatureUpdate = "FeatureOptions"
getTagUpdate GatewayBankEmiSupportUpdate = "GatewayBankEmiSupportOptions"
getTagUpdate AuthenticationAccountUpdate = "AuthenticationAccountOptions"
getTagUpdate PaymentGatewayResponseV1Update = "PaymentGatewayResponseV1Options"
getTagUpdate SavedPaymentMethodUpdate = "SavedPaymentMethodOptions"
getTagUpdate MerchantProviderDetailsUpdate = "MerchantProviderDetailsOptions"
getTagUpdate TxnOfferUpdate = "TxnOfferOptions"
getTagUpdate GatewayHealthUpdate = "GatewayHealthOptions"
getTagUpdate RiskManagementAccountUpdate = "RiskManagementAccountOptions"
getTagUpdate CardInfoUpdate = "CardInfoOptions"
getTagUpdate DeviceBindingUpdate = "DeviceBindingOptions"
getTagUpdate NotificationUpdate = "NotificationOptions"
getTagUpdate OrderBasketUpdate = "OrderBasketOptions"
getTagUpdate GatewayPaymentMethodUpdate = "GatewayPaymentMethodOptions"
getTagUpdate ProcessTrackerUpdate = "ProcessTrackerOptions"
getTagUpdate PaymentLinksUpdate = "PaymentLinksOptions"
getTagUpdate CustomerAccountUpdate = "CustomerAccountOptions"
getTagUpdate EntityMapUpdate = "EntityMapOptions"
getTagUpdate FormInputUpdate = "FormInputOptions"
getTagUpdate IngressRuleUpdate = "IngressRuleOptions"
getTagUpdate InstallmentUpdate = "InstallmentOptions"
getTagUpdate InstallmentRefundUpdate = "InstallmentRefundOptions"
getTagUpdate PaymentFormUpdate = "PaymentFormOptions"
getTagUpdate UserRoleUpdate = "UserRoleOptions"
getTagUpdate TxnIntentDetailUpdate = "TxnIntentDetailOptions"
getTagUpdate AuthMappingUpdate = "AuthMappingOptions"

parseTagUpdate :: Text -> Parser UpdateModel
parseTagUpdate "TxnOfferInfoOptions" = return TxnOfferInfoUpdate
parseTagUpdate "JuspayEventOptions" = return JuspayEventUpdate
parseTagUpdate "HdfcHashedNumOptions" = return HdfcHashedNumUpdate
parseTagUpdate "MerchantGatewayAccountOptions" = return MerchantGatewayAccountUpdate
parseTagUpdate "RuleOptions" = return RuleUpdate
parseTagUpdate "MerchantGatewayAccountSubInfoOptions" = return MerchantGatewayAccountSubInfoUpdate
parseTagUpdate "OrderReferenceOptions" = return OrderReferenceUpdate
parseTagUpdate "MandateOptions" = return MandateUpdate
parseTagUpdate "GatewayTxnDataOptions" = return GatewayTxnDataUpdate
parseTagUpdate "MerchantGatewayCardInfoOptions" = return MerchantGatewayCardInfoUpdate
parseTagUpdate "TxnRiskCheckOptions" = return TxnRiskCheckUpdate
parseTagUpdate "PaymentMethodOptions" = return PaymentMethodUpdate
parseTagUpdate "OfferBenefitInfoOptions" = return OfferBenefitInfoUpdate
parseTagUpdate "MerchantIframePreferencesOptions" = return MerchantIframePreferencesUpdate
parseTagUpdate "ResellerAccountOptions" = return ResellerAccountUpdate
parseTagUpdate "UnifiedGatewayResponseOptions" = return UnifiedGatewayResponseUpdate
parseTagUpdate "WalletTopUpTxnOptions" = return WalletTopUpTxnUpdate
parseTagUpdate "IsinRoutesOptions" = return IsinRoutesUpdate
parseTagUpdate "TxnCardInfoOptions" = return TxnCardInfoUpdate
parseTagUpdate "OrderAddressOptions" = return OrderAddressUpdate
parseTagUpdate "LockerAccountOptions" = return LockerAccountUpdate
parseTagUpdate "JuspayBankCodeOptions" = return JuspayBankCodeUpdate
parseTagUpdate "IssuerRoutesOptions" = return IssuerRoutesUpdate
parseTagUpdate "RefundOptions" = return RefundUpdate
parseTagUpdate "SecondFactorOptions" = return SecondFactorUpdate
parseTagUpdate "GatewayOutageOptions" = return GatewayOutageUpdate
parseTagUpdate "TxnDetailOptions" = return TxnDetailUpdate
parseTagUpdate "EmiPlanOptions" = return EmiPlanUpdate
parseTagUpdate "MerchantKeyOptions" = return MerchantKeyUpdate
parseTagUpdate "NetworkCardFingerprintOptions" = return NetworkCardFingerprintUpdate
parseTagUpdate "TokenRequestorOptions" = return TokenRequestorUpdate
parseTagUpdate "TxnOfferDetailOptions" = return TxnOfferDetailUpdate
parseTagUpdate "SecondFactorResponseOptions" = return SecondFactorResponseUpdate
parseTagUpdate "OfferRedemptionOptions" = return OfferRedemptionUpdate
parseTagUpdate "CardBrandRoutesOptions" = return CardBrandRoutesUpdate
parseTagUpdate "CustomerOptions" = return CustomerUpdate
parseTagUpdate "MerchantAccountOptions" = return MerchantAccountUpdate
parseTagUpdate "ExternalMerchantCustomerOptions" = return ExternalMerchantCustomerUpdate
parseTagUpdate "TokenBinInfoOptions" = return TokenBinInfoUpdate
parseTagUpdate "MerchantGatewayPaymentMethodOptions" = return MerchantGatewayPaymentMethodUpdate
parseTagUpdate "PromotionOptions" = return PromotionUpdate
parseTagUpdate "LockerTokenRequestorOptions" = return LockerTokenRequestorUpdate
parseTagUpdate "BankAccountOptions" = return BankAccountUpdate
parseTagUpdate "AgencyOptions" = return AgencyUpdate
parseTagUpdate "ProviderOptions" = return ProviderUpdate
parseTagUpdate "GatewayCardInfoOptions" = return GatewayCardInfoUpdate
parseTagUpdate "PaymentGatewayResponseOptions" = return PaymentGatewayResponseUpdate
parseTagUpdate "MetadataOptions" = return MetadataUpdate
parseTagUpdate "ChargebackOptions" = return ChargebackUpdate
parseTagUpdate "WalletAccountOptions" = return WalletAccountUpdate
parseTagUpdate "GatewayStatusMapOptions" = return GatewayStatusMapUpdate
parseTagUpdate "TokenOptions" = return TokenUpdate
parseTagUpdate "MerchantLockerAccountOptions" = return MerchantLockerAccountUpdate
parseTagUpdate "TempCardOptions" = return TempCardUpdate
parseTagUpdate "MerchantRiskSettingsOptions" = return MerchantRiskSettingsUpdate
parseTagUpdate "UserOptions" = return UserUpdate
parseTagUpdate "CofDetailsOptions" = return CofDetailsUpdate
parseTagUpdate "OrderMetadataV2Options" = return OrderMetadataV2Update
parseTagUpdate "StoredCardOptions" = return StoredCardUpdate
parseTagUpdate "TokenCustomerOptions" = return TokenCustomerUpdate
parseTagUpdate "EnrolledPanOptions" = return EnrolledPanUpdate
parseTagUpdate "OffersOptions" = return OffersUpdate
parseTagUpdate "RoleOptions" = return RoleUpdate
parseTagUpdate "FeatureOptions" = return FeatureUpdate
parseTagUpdate "GatewayBankEmiSupportOptions" = return GatewayBankEmiSupportUpdate
parseTagUpdate "AuthenticationAccountOptions" = return AuthenticationAccountUpdate
parseTagUpdate "PaymentGatewayResponseV1Options" = return PaymentGatewayResponseV1Update
parseTagUpdate "SavedPaymentMethodOptions" = return SavedPaymentMethodUpdate
parseTagUpdate "MerchantProviderDetailsOptions" = return MerchantProviderDetailsUpdate
parseTagUpdate "TxnOfferOptions" = return TxnOfferUpdate
parseTagUpdate "GatewayHealthOptions" = return GatewayHealthUpdate
parseTagUpdate "RiskManagementAccountOptions" = return RiskManagementAccountUpdate
parseTagUpdate "CardInfoOptions" = return CardInfoUpdate
parseTagUpdate "DeviceBindingOptions" = return DeviceBindingUpdate
parseTagUpdate "NotificationOptions" = return NotificationUpdate
parseTagUpdate "OrderBasketOptions" = return OrderBasketUpdate
parseTagUpdate "GatewayPaymentMethodOptions" = return GatewayPaymentMethodUpdate
parseTagUpdate "ProcessTrackerOptions" = return ProcessTrackerUpdate
parseTagUpdate "PaymentLinksOptions" = return PaymentLinksUpdate
parseTagUpdate "CustomerAccountOptions" = return CustomerAccountUpdate
parseTagUpdate "EntityMapOptions" = return EntityMapUpdate
parseTagUpdate "FormInputOptions" = return FormInputUpdate
parseTagUpdate "IngressRuleOptions" = return IngressRuleUpdate
parseTagUpdate "InstallmentOptions" = return InstallmentUpdate
parseTagUpdate "InstallmentRefundOptions" = return InstallmentRefundUpdate
parseTagUpdate "PaymentFormOptions" = return PaymentFormUpdate
parseTagUpdate "UserRoleOptions" = return UserRoleUpdate
parseTagUpdate "TxnIntentDetailOptions" = return TxnIntentDetailUpdate
parseTagUpdate "AuthMappingOptions" = return AuthMappingUpdate
parseTagUpdate t = fail $ T.unpack ("Expected a UpdateModel but got '" <> t <> "'")

data DBUpdateObject
  = TxnOfferInfoOptions UpdateModel [Set MySQL TxnOfferInfoT] (Where MySQL TxnOfferInfoT)
  | JuspayEventOptions UpdateModel [Set MySQL JuspayEventT] (Where MySQL JuspayEventT)
  | HdfcHashedNumOptions UpdateModel [Set MySQL HdfcHashedNumT] (Where MySQL HdfcHashedNumT)
  | MerchantGatewayAccountOptions UpdateModel [Set MySQL MerchantGatewayAccountT] (Where MySQL MerchantGatewayAccountT)
  | MerchantGatewayAccountSubInfoOptions UpdateModel [Set MySQL MerchantGatewayAccountSubInfoT] (Where MySQL MerchantGatewayAccountSubInfoT)
  | OrderReferenceOptions UpdateModel [Set MySQL OrderReferenceT] (Where MySQL OrderReferenceT)
  | MandateOptions UpdateModel [Set MySQL MandateT] (Where MySQL MandateT)
  | GatewayTxnDataOptions UpdateModel [Set MySQL GatewayTxnDataT] (Where MySQL GatewayTxnDataT)
  | MerchantGatewayCardInfoOptions UpdateModel [Set MySQL MerchantGatewayCardInfoT] (Where MySQL MerchantGatewayCardInfoT)
  | TxnRiskCheckOptions UpdateModel [Set MySQL TxnRiskCheckT] (Where MySQL TxnRiskCheckT)
  | PaymentMethodOptions UpdateModel [Set MySQL PaymentMethodT] (Where MySQL PaymentMethodT)
  | OfferBenefitInfoOptions UpdateModel [Set MySQL OfferBenefitInfoT] (Where MySQL OfferBenefitInfoT)
  | MerchantIframePreferencesOptions UpdateModel [Set MySQL MerchantIframePreferencesT] (Where MySQL MerchantIframePreferencesT)
  | ResellerAccountOptions UpdateModel [Set MySQL ResellerAccountT] (Where MySQL ResellerAccountT)
  | UnifiedGatewayResponseOptions UpdateModel [Set MySQL UnifiedGatewayResponseT] (Where MySQL UnifiedGatewayResponseT)
  | WalletTopUpTxnOptions UpdateModel [Set MySQL WalletTopUpTxnT] (Where MySQL WalletTopUpTxnT)
  | IsinRoutesOptions UpdateModel [Set MySQL IsinRoutesT] (Where MySQL IsinRoutesT)
  | TxnCardInfoOptions UpdateModel [Set MySQL TxnCardInfoT] (Where MySQL TxnCardInfoT)
  | OrderAddressOptions UpdateModel [Set MySQL OrderAddressT] (Where MySQL OrderAddressT)
  | LockerAccountOptions UpdateModel [Set MySQL LockerAccountT] (Where MySQL LockerAccountT)
  | JuspayBankCodeOptions UpdateModel [Set MySQL JuspayBankCodeT] (Where MySQL JuspayBankCodeT)
  | IssuerRoutesOptions UpdateModel [Set MySQL IssuerRoutesT] (Where MySQL IssuerRoutesT)
  | RefundOptions UpdateModel [Set MySQL RefundT] (Where MySQL RefundT)
  | SecondFactorOptions UpdateModel [Set MySQL SecondFactorT] (Where MySQL SecondFactorT)
  | GatewayOutageOptions UpdateModel [Set MySQL GatewayOutageT] (Where MySQL GatewayOutageT)
  | TxnDetailOptions UpdateModel [Set MySQL TxnDetailT] (Where MySQL TxnDetailT)
  | EmiPlanOptions UpdateModel [Set MySQL EmiPlanT] (Where MySQL EmiPlanT)
  | MerchantKeyOptions UpdateModel [Set MySQL MerchantKeyT] (Where MySQL MerchantKeyT)
  | NetworkCardFingerprintOptions UpdateModel [Set MySQL NetworkCardFingerprintT] (Where MySQL NetworkCardFingerprintT)
  | TokenRequestorOptions UpdateModel [Set MySQL TokenRequestorT] (Where MySQL TokenRequestorT)
  | TxnOfferDetailOptions UpdateModel [Set MySQL TxnOfferDetailT] (Where MySQL TxnOfferDetailT)
  | SecondFactorResponseOptions UpdateModel [Set MySQL SecondFactorResponseT] (Where MySQL SecondFactorResponseT)
  | CardBrandRoutesOptions UpdateModel [Set MySQL CardBrandRoutesT] (Where MySQL CardBrandRoutesT)
  | CustomerOptions UpdateModel [Set MySQL CustomerT] (Where MySQL CustomerT)
  | MerchantAccountOptions UpdateModel [Set MySQL MerchantAccountT] (Where MySQL MerchantAccountT)
  | TokenBinInfoOptions UpdateModel [Set MySQL TokenBinInfoT] (Where MySQL TokenBinInfoT)
  | MerchantGatewayPaymentMethodOptions UpdateModel [Set MySQL MerchantGatewayPaymentMethodT] (Where MySQL MerchantGatewayPaymentMethodT)
  | PromotionOptions UpdateModel [Set MySQL PromotionT] (Where MySQL PromotionT)
  | LockerTokenRequestorOptions UpdateModel [Set MySQL LockerTokenRequestorT] (Where MySQL LockerTokenRequestorT)
  | BankAccountOptions UpdateModel [Set MySQL BankAccountT] (Where MySQL BankAccountT)
  | ProviderOptions UpdateModel [Set MySQL ProviderT] (Where MySQL ProviderT)
  | GatewayCardInfoOptions UpdateModel [Set MySQL GatewayCardInfoT] (Where MySQL GatewayCardInfoT)
  | PaymentGatewayResponseOptions UpdateModel [Set MySQL PaymentGatewayResponseT] (Where MySQL PaymentGatewayResponseT)
  | MetadataOptions UpdateModel [Set MySQL MetadataT] (Where MySQL MetadataT)
  | ChargebackOptions UpdateModel [Set MySQL ChargebackT] (Where MySQL ChargebackT)
  | WalletAccountOptions UpdateModel [Set MySQL WalletAccountT] (Where MySQL WalletAccountT)
  | GatewayStatusMapOptions UpdateModel [Set MySQL GatewayStatusMapT] (Where MySQL GatewayStatusMapT)
  | TokenOptions UpdateModel [Set MySQL TokenT] (Where MySQL TokenT)
  | MerchantLockerAccountOptions UpdateModel [Set MySQL MerchantLockerAccountT] (Where MySQL MerchantLockerAccountT)
  | TempCardOptions UpdateModel [Set MySQL TempCardT] (Where MySQL TempCardT)
  | MerchantRiskSettingsOptions UpdateModel [Set MySQL MerchantRiskSettingsT] (Where MySQL MerchantRiskSettingsT)
  | UserOptions UpdateModel [Set MySQL UserT] (Where MySQL UserT)
  | CofDetailsOptions UpdateModel [Set MySQL CofDetailsT] (Where MySQL CofDetailsT)
  | OrderMetadataV2Options UpdateModel [Set MySQL OrderMetadataV2T] (Where MySQL OrderMetadataV2T)
  | StoredCardOptions UpdateModel [Set MySQL StoredCardT] (Where MySQL StoredCardT)
  | TokenCustomerOptions UpdateModel [Set MySQL TokenCustomerT] (Where MySQL TokenCustomerT)
  | EnrolledPanOptions UpdateModel [Set MySQL EnrolledPanT] (Where MySQL EnrolledPanT)
  | RoleOptions UpdateModel [Set MySQL RoleT] (Where MySQL RoleT)
  | FeatureOptions UpdateModel [Set MySQL FeatureT] (Where MySQL FeatureT)
  | GatewayBankEmiSupportOptions UpdateModel [Set MySQL GatewayBankEmiSupportT] (Where MySQL GatewayBankEmiSupportT)
  | AuthenticationAccountOptions UpdateModel [Set MySQL AuthenticationAccountT] (Where MySQL AuthenticationAccountT)
  | PaymentGatewayResponseV1Options UpdateModel [Set MySQL PaymentGatewayResponseV1T] (Where MySQL PaymentGatewayResponseV1T)
  | MerchantProviderDetailsOptions UpdateModel [Set MySQL MerchantProviderDetailsT] (Where MySQL MerchantProviderDetailsT)
  | TxnOfferOptions UpdateModel [Set MySQL TxnOfferT] (Where MySQL TxnOfferT)
  | GatewayHealthOptions UpdateModel [Set MySQL GatewayHealthT] (Where MySQL GatewayHealthT)
  | RiskManagementAccountOptions UpdateModel [Set MySQL RiskManagementAccountT] (Where MySQL RiskManagementAccountT)
  | CardInfoOptions UpdateModel [Set MySQL CardInfoT] (Where MySQL CardInfoT)
  | DeviceBindingOptions UpdateModel [Set MySQL DeviceBindingT] (Where MySQL DeviceBindingT)
  | NotificationOptions UpdateModel [Set MySQL NotificationT] (Where MySQL NotificationT)
  | OrderBasketOptions UpdateModel [Set MySQL OrderBasketT] (Where MySQL OrderBasketT)
  | GatewayPaymentMethodOptions UpdateModel [Set MySQL GatewayPaymentMethodT] (Where MySQL GatewayPaymentMethodT)
  | PaymentLinksOptions UpdateModel [Set MySQL PaymentLinksT] (Where MySQL PaymentLinksT)
  | CustomerAccountOptions UpdateModel [Set MySQL CustomerAccountT] (Where MySQL CustomerAccountT)
  | EntityMapOptions UpdateModel [Set MySQL EntityMapT] (Where MySQL EntityMapT)
  | FormInputOptions UpdateModel [Set MySQL FormInputT] (Where MySQL FormInputT)
  | IngressRuleOptions UpdateModel [Set MySQL IngressRuleT] (Where MySQL IngressRuleT)
  | InstallmentOptions UpdateModel [Set MySQL InstallmentT] (Where MySQL InstallmentT)
  | InstallmentRefundOptions UpdateModel [Set MySQL InstallmentRefundT] (Where MySQL InstallmentRefundT)
  | PaymentFormOptions UpdateModel [Set MySQL PaymentFormT] (Where MySQL PaymentFormT)
  | UserRoleOptions UpdateModel [Set MySQL UserRoleT] (Where MySQL UserRoleT)
  | TxnIntentDetailOptions UpdateModel [Set MySQL TxnIntentDetailT] (Where MySQL TxnIntentDetailT)
  | AuthMappingOptions UpdateModel [Set MySQL AuthMappingT] (Where MySQL AuthMappingT)
  | OffersOptions UpdateModel [Set Postgres OffersT] (Where Postgres OffersT)
  | ExternalMerchantCustomerOptions UpdateModel [Set Postgres ExternalMerchantCustomerT] (Where Postgres ExternalMerchantCustomerT)
  | RuleOptions UpdateModel [Set Postgres RuleT] (Where Postgres RuleT)
  | OfferRedemptionOptions UpdateModel [Set Postgres OfferRedemptionT] (Where Postgres OfferRedemptionT)
  | AgencyOptions UpdateModel [Set Postgres AgencyT] (Where Postgres AgencyT)
  | SavedPaymentMethodOptions UpdateModel [Set Postgres SavedPaymentMethodT] (Where Postgres SavedPaymentMethodT)
  | ProcessTrackerOptions UpdateModel [Set Postgres ProcessTrackerT] (Where Postgres ProcessTrackerT)

-------------------------------- ToJSON DBUpdateObject -------------------------------------
instance ToJSON DBUpdateObject where
  toJSON = error "ToJSON not implemented for DBUpdateObject - Use getDbUpdateCommandJson instead" -- Using getDbUpdateCommandJson instead of toJSON

-------------------------------- FromJSON DBUpdateObject -----------------------------------
instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contents <- o .: "contents"
    updateModel <- parseTagUpdate =<< (o .: "tag")
    case updateModel of
      TxnOfferInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnOfferInfoOptions updateModel updVals whereClause
      JuspayEventUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ JuspayEventOptions updateModel updVals whereClause
      HdfcHashedNumUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ HdfcHashedNumOptions updateModel updVals whereClause
      MerchantGatewayAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantGatewayAccountOptions updateModel updVals whereClause
      RuleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RuleOptions updateModel updVals whereClause
      MerchantGatewayAccountSubInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantGatewayAccountSubInfoOptions updateModel updVals whereClause
      OrderReferenceUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OrderReferenceOptions updateModel updVals whereClause
      MandateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MandateOptions updateModel updVals whereClause
      GatewayTxnDataUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayTxnDataOptions updateModel updVals whereClause
      MerchantGatewayCardInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantGatewayCardInfoOptions updateModel updVals whereClause
      TxnRiskCheckUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnRiskCheckOptions updateModel updVals whereClause
      PaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentMethodOptions updateModel updVals whereClause
      OfferBenefitInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OfferBenefitInfoOptions updateModel updVals whereClause
      MerchantIframePreferencesUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantIframePreferencesOptions updateModel updVals whereClause
      ResellerAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ResellerAccountOptions updateModel updVals whereClause
      UnifiedGatewayResponseUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ UnifiedGatewayResponseOptions updateModel updVals whereClause
      WalletTopUpTxnUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ WalletTopUpTxnOptions updateModel updVals whereClause
      IsinRoutesUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IsinRoutesOptions updateModel updVals whereClause
      TxnCardInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnCardInfoOptions updateModel updVals whereClause
      OrderAddressUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OrderAddressOptions updateModel updVals whereClause
      LockerAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ LockerAccountOptions updateModel updVals whereClause
      JuspayBankCodeUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ JuspayBankCodeOptions updateModel updVals whereClause
      IssuerRoutesUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssuerRoutesOptions updateModel updVals whereClause
      RefundUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RefundOptions updateModel updVals whereClause
      SecondFactorUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SecondFactorOptions updateModel updVals whereClause
      GatewayOutageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayOutageOptions updateModel updVals whereClause
      TxnDetailUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnDetailOptions updateModel updVals whereClause
      EmiPlanUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EmiPlanOptions updateModel updVals whereClause
      MerchantKeyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantKeyOptions updateModel updVals whereClause
      NetworkCardFingerprintUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ NetworkCardFingerprintOptions updateModel updVals whereClause
      TokenRequestorUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TokenRequestorOptions updateModel updVals whereClause
      TxnOfferDetailUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnOfferDetailOptions updateModel updVals whereClause
      SecondFactorResponseUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SecondFactorResponseOptions updateModel updVals whereClause
      OfferRedemptionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OfferRedemptionOptions updateModel updVals whereClause
      CardBrandRoutesUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CardBrandRoutesOptions updateModel updVals whereClause
      CustomerUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CustomerOptions updateModel updVals whereClause
      MerchantAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantAccountOptions updateModel updVals whereClause
      ExternalMerchantCustomerUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ExternalMerchantCustomerOptions updateModel updVals whereClause
      TokenBinInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TokenBinInfoOptions updateModel updVals whereClause
      MerchantGatewayPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantGatewayPaymentMethodOptions updateModel updVals whereClause
      PromotionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PromotionOptions updateModel updVals whereClause
      LockerTokenRequestorUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ LockerTokenRequestorOptions updateModel updVals whereClause
      BankAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BankAccountOptions updateModel updVals whereClause
      AgencyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AgencyOptions updateModel updVals whereClause
      ProviderUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ProviderOptions updateModel updVals whereClause
      GatewayCardInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayCardInfoOptions updateModel updVals whereClause
      PaymentGatewayResponseUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentGatewayResponseOptions updateModel updVals whereClause
      MetadataUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MetadataOptions updateModel updVals whereClause
      ChargebackUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ChargebackOptions updateModel updVals whereClause
      WalletAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ WalletAccountOptions updateModel updVals whereClause
      GatewayStatusMapUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayStatusMapOptions updateModel updVals whereClause
      TokenUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TokenOptions updateModel updVals whereClause
      MerchantLockerAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantLockerAccountOptions updateModel updVals whereClause
      TempCardUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TempCardOptions updateModel updVals whereClause
      MerchantRiskSettingsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantRiskSettingsOptions updateModel updVals whereClause
      UserUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ UserOptions updateModel updVals whereClause
      CofDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CofDetailsOptions updateModel updVals whereClause
      OrderMetadataV2Update -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OrderMetadataV2Options updateModel updVals whereClause
      StoredCardUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ StoredCardOptions updateModel updVals whereClause
      TokenCustomerUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TokenCustomerOptions updateModel updVals whereClause
      EnrolledPanUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EnrolledPanOptions updateModel updVals whereClause
      OffersUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OffersOptions updateModel updVals whereClause
      RoleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RoleOptions updateModel updVals whereClause
      FeatureUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FeatureOptions updateModel updVals whereClause
      GatewayBankEmiSupportUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayBankEmiSupportOptions updateModel updVals whereClause
      AuthenticationAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AuthenticationAccountOptions updateModel updVals whereClause
      PaymentGatewayResponseV1Update -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentGatewayResponseV1Options updateModel updVals whereClause
      SavedPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SavedPaymentMethodOptions updateModel updVals whereClause
      MerchantProviderDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantProviderDetailsOptions updateModel updVals whereClause
      TxnOfferUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnOfferOptions updateModel updVals whereClause
      GatewayHealthUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayHealthOptions updateModel updVals whereClause
      RiskManagementAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RiskManagementAccountOptions updateModel updVals whereClause
      CardInfoUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CardInfoOptions updateModel updVals whereClause
      DeviceBindingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DeviceBindingOptions updateModel updVals whereClause
      NotificationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ NotificationOptions updateModel updVals whereClause
      OrderBasketUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OrderBasketOptions updateModel updVals whereClause
      GatewayPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GatewayPaymentMethodOptions updateModel updVals whereClause
      ProcessTrackerUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ProcessTrackerOptions updateModel updVals whereClause
      PaymentLinksUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentLinksOptions updateModel updVals whereClause
      CustomerAccountUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CustomerAccountOptions updateModel updVals whereClause
      EntityMapUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EntityMapOptions updateModel updVals whereClause
      FormInputUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FormInputOptions updateModel updVals whereClause
      IngressRuleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IngressRuleOptions updateModel updVals whereClause
      InstallmentUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ InstallmentOptions updateModel updVals whereClause
      InstallmentRefundUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ InstallmentRefundOptions updateModel updVals whereClause
      PaymentFormUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentFormOptions updateModel updVals whereClause
      UserRoleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ UserRoleOptions updateModel updVals whereClause
      TxnIntentDetailUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TxnIntentDetailOptions updateModel updVals whereClause
      AuthMappingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AuthMappingOptions updateModel updVals whereClause
