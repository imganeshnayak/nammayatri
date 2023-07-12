{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types.DBSync.Delete where

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

data DeleteModel
  = TxnOfferInfoDelete
  | TestTableDelete
  | HdfcHashedNumDelete
  | MerchantGatewayAccountDelete
  | BankInfoDelete
  | RuleDelete
  | MerchantProviderCofDetailsDelete
  | MerchantCustomerCofDetailsDelete
  | MerchantGatewayAccountSubInfoDelete
  | MandateDelete
  | OrderReferenceDelete
  | MerchantGatewayCardInfoDelete
  | GatewayTxnDataDelete
  | PaymentMethodDelete
  | TxnRiskCheckDelete
  | MerchantIframePreferencesDelete
  | OfferBenefitInfoDelete
  | UnifiedGatewayResponseDelete
  | ResellerAccountDelete
  | IsinRoutesDelete
  | WalletTopUpTxnDelete
  | OrderAddressDelete
  | TxnCardInfoDelete
  | JuspayBankCodeDelete
  | LockerAccountDelete
  | IssuerRoutesDelete
  | MerchantPriorityLogicDelete
  | SecondFactorDelete
  | RefundDelete
  | TxnDetailDelete
  | GatewayOutageDelete
  | MerchantKeyDelete
  | EmiPlanDelete
  | NetworkCardFingerprintDelete
  | TxnOfferDetailDelete
  | TokenRequestorDelete
  | EncryptionKeysDelete
  | SecondFactorResponseDelete
  | CardBrandRoutesDelete
  | OfferRedemptionDelete
  | MerchantAccountDelete
  | CustomerDelete
  | TokenBinInfoDelete
  | ExternalMerchantCustomerDelete
  | PromotionDelete
  | MerchantGatewayPaymentMethodDelete
  | BankAccountDelete
  | LockerTokenRequestorDelete
  | ProviderDelete
  | AgencyDelete
  | GatewayCardInfoDelete
  | MetadataDelete
  | PaymentGatewayResponseDelete
  | JuspayErrorMapDelete
  | GatewayTxnStatusMapDelete
  | ChargebackDelete
  | ConfigurationsDelete
  | ProcessTrackerDelete
  | GatewayStatusMapDelete
  | WalletAccountDelete
  | MerchantLockerAccountDelete
  | TokenDelete
  | TempCardDelete
  | JuspayEventDelete
  | UserDelete
  | MerchantRiskSettingsDelete
  | JuspayIssuerNameMappingDelete
  | CofDetailsDelete
  | OrderMetadataV2Delete
  | ServiceConfigurationDelete
  | TokenCustomerDelete
  | StoredCardDelete
  | EnrolledPanDelete
  | PaymentLinksDelete
  | OffersDelete
  | FeatureDelete
  | RoleDelete
  | AuthenticationAccountDelete
  | GatewayBankEmiSupportDelete
  | SavedPaymentMethodDelete
  | PaymentGatewayResponseV1Delete
  | TxnOfferDelete
  | MerchantProviderDetailsDelete
  | RiskManagementAccountDelete
  | GatewayHealthDelete
  | DeviceBindingDelete
  | CardInfoDelete
  | OrderBasketDelete
  | NotificationDelete
  | GatewayPaymentMethodDelete
  | CustomerAccountDelete
  | EntityMapDelete
  | FormInputDelete
  | IngressRuleDelete
  | InstallmentDelete
  | InstallmentRefundDelete
  | PaymentFormDelete
  | UserRoleDelete
  | TxnIntentDetailDelete
  | AuthMappingDelete
  deriving (Generic, Show)

getTagDelete :: DeleteModel -> Text
getTagDelete TxnOfferInfoDelete = "TxnOfferInfoOptions"
getTagDelete TestTableDelete = " TestTableOptions"
getTagDelete JuspayEventDelete = "JuspayEventOptions"
getTagDelete HdfcHashedNumDelete = "HdfcHashedNumOptions"
getTagDelete MerchantGatewayAccountDelete = "MerchantGatewayAccountOptions"
getTagDelete BankInfoDelete = "BankInfoOptions"
getTagDelete RuleDelete = "RuleOptions"
getTagDelete MerchantProviderCofDetailsDelete = "MerchantProviderCofDetailsOptions"
getTagDelete MerchantCustomerCofDetailsDelete = "MerchantCustomerCofDetailsOptions"
getTagDelete MerchantGatewayAccountSubInfoDelete = "MerchantGatewayAccountSubInfoOptions"
getTagDelete OrderReferenceDelete = "OrderReferenceOptions"
getTagDelete MandateDelete = "MandateOptions"
getTagDelete GatewayTxnDataDelete = "GatewayTxnDataOptions"
getTagDelete MerchantGatewayCardInfoDelete = "MerchantGatewayCardInfoOptions"
getTagDelete TxnRiskCheckDelete = "TxnRiskCheckOptions"
getTagDelete PaymentMethodDelete = "PaymentMethodOptions"
getTagDelete OfferBenefitInfoDelete = "OfferBenefitInfoOptions"
getTagDelete MerchantIframePreferencesDelete = "MerchantIframePreferencesOptions"
getTagDelete ResellerAccountDelete = "ResellerAccountOptions"
getTagDelete UnifiedGatewayResponseDelete = "UnifiedGatewayResponseOptions"
getTagDelete WalletTopUpTxnDelete = "WalletTopUpTxnOptions"
getTagDelete IsinRoutesDelete = "IsinRoutesOptions"
getTagDelete TxnCardInfoDelete = "TxnCardInfoOptions"
getTagDelete OrderAddressDelete = "OrderAddressOptions"
getTagDelete LockerAccountDelete = "LockerAccountOptions"
getTagDelete JuspayBankCodeDelete = "JuspayBankCodeOptions"
getTagDelete IssuerRoutesDelete = "IssuerRoutesOptions"
getTagDelete MerchantPriorityLogicDelete = "MerchantPriorityLogicOptions"
getTagDelete RefundDelete = "RefundOptions"
getTagDelete SecondFactorDelete = "SecondFactorOptions"
getTagDelete GatewayOutageDelete = "GatewayOutageOptions"
getTagDelete JuspayErrorMapDelete = "JuspayErrorMapOptions"
getTagDelete TxnDetailDelete = "TxnDetailOptions"
getTagDelete EmiPlanDelete = "EmiPlanOptions"
getTagDelete MerchantKeyDelete = "MerchantKeyOptions"
getTagDelete NetworkCardFingerprintDelete = "NetworkCardFingerprintOptions"
getTagDelete TokenRequestorDelete = "TokenRequestorOptions"
getTagDelete TxnOfferDetailDelete = "TxnOfferDetailOptions"
getTagDelete SecondFactorResponseDelete = "SecondFactorResponseOptions"
getTagDelete EncryptionKeysDelete = "EncryptionKeysOptions"
getTagDelete OfferRedemptionDelete = "OfferRedemptionOptions"
getTagDelete CardBrandRoutesDelete = "CardBrandRoutesOptions"
getTagDelete CustomerDelete = "CustomerOptions"
getTagDelete MerchantAccountDelete = "MerchantAccountOptions"
getTagDelete ExternalMerchantCustomerDelete = "ExternalMerchantCustomerOptions"
getTagDelete TokenBinInfoDelete = "TokenBinInfoOptions"
getTagDelete MerchantGatewayPaymentMethodDelete = "MerchantGatewayPaymentMethodOptions"
getTagDelete PromotionDelete = "PromotionOptions"
getTagDelete LockerTokenRequestorDelete = "LockerTokenRequestorOptions"
getTagDelete BankAccountDelete = "BankAccountOptions"
getTagDelete AgencyDelete = "AgencyOptions"
getTagDelete ProviderDelete = "ProviderOptions"
getTagDelete GatewayCardInfoDelete = "GatewayCardInfoOptions"
getTagDelete PaymentGatewayResponseDelete = "PaymentGatewayResponseOptions"
getTagDelete MetadataDelete = "MetadataOptions"
getTagDelete GatewayTxnStatusMapDelete = "GatewayTxnStatusMapOptions"
getTagDelete ChargebackDelete = "ChargebackOptions"
getTagDelete WalletAccountDelete = "WalletAccountOptions"
getTagDelete GatewayStatusMapDelete = "GatewayStatusMapOptions"
getTagDelete TokenDelete = "TokenOptions"
getTagDelete MerchantLockerAccountDelete = "MerchantLockerAccountOptions"
getTagDelete TempCardDelete = "TempCardOptions"
getTagDelete MerchantRiskSettingsDelete = "MerchantRiskSettingsOptions"
getTagDelete UserDelete = "UserOptions"
getTagDelete CofDetailsDelete = "CofDetailsOptions"
getTagDelete JuspayIssuerNameMappingDelete = "JuspayIssuerNameMappingOptions"
getTagDelete ServiceConfigurationDelete = "ServiceConfigurationOptions"
getTagDelete OrderMetadataV2Delete = "OrderMetadataV2Options"
getTagDelete StoredCardDelete = "StoredCardOptions"
getTagDelete TokenCustomerDelete = "TokenCustomerOptions"
getTagDelete EnrolledPanDelete = "EnrolledPanOptions"
getTagDelete OffersDelete = "OffersOptions"
getTagDelete PaymentLinksDelete = "PaymentLinksOptions"
getTagDelete RoleDelete = "RoleOptions"
getTagDelete FeatureDelete = "FeatureOptions"
getTagDelete GatewayBankEmiSupportDelete = "GatewayBankEmiSupportOptions"
getTagDelete AuthenticationAccountDelete = "AuthenticationAccountOptions"
getTagDelete PaymentGatewayResponseV1Delete = "PaymentGatewayResponseV1Options"
getTagDelete SavedPaymentMethodDelete = "SavedPaymentMethodOptions"
getTagDelete MerchantProviderDetailsDelete = "MerchantProviderDetailsOptions"
getTagDelete TxnOfferDelete = "TxnOfferOptions"
getTagDelete GatewayHealthDelete = "GatewayHealthOptions"
getTagDelete RiskManagementAccountDelete = "RiskManagementAccountOptions"
getTagDelete CardInfoDelete = "CardInfoOptions"
getTagDelete DeviceBindingDelete = "DeviceBindingOptions"
getTagDelete NotificationDelete = "NotificationOptions"
getTagDelete OrderBasketDelete = "OrderBasketOptions"
getTagDelete GatewayPaymentMethodDelete = "GatewayPaymentMethodOptions"
getTagDelete ProcessTrackerDelete = "ProcessTrackerOptions"
getTagDelete ConfigurationsDelete = "ConfigurationsOptions"
getTagDelete CustomerAccountDelete = "CustomerAccountOptions"
getTagDelete EntityMapDelete = "EntityMapOptions"
getTagDelete FormInputDelete = "FormInputOptions"
getTagDelete IngressRuleDelete = "IngressRuleOptions"
getTagDelete InstallmentDelete = "InstallmentOptions"
getTagDelete InstallmentRefundDelete = "InstallmentRefundOptions"
getTagDelete PaymentFormDelete = "PaymentFormOptions"
getTagDelete UserRoleDelete = "UserRoleOptions"
getTagDelete TxnIntentDetailDelete = "TxnIntentDetailOptions"
getTagDelete AuthMappingDelete = "AuthMappingOptions"

parseTagDelete :: Text -> Parser DeleteModel
parseTagDelete "TxnOfferInfoOptions" = return TxnOfferInfoDelete
parseTagDelete "TestTableOptions" = return TestTableDelete
parseTagDelete "HdfcHashedNumOptions" = return HdfcHashedNumDelete
parseTagDelete "MerchantGatewayAccountOptions" = return MerchantGatewayAccountDelete
parseTagDelete "BankInfoOptions" = return BankInfoDelete
parseTagDelete "MerchantProviderCofDetailsOptions" = return MerchantProviderCofDetailsDelete
parseTagDelete "RuleOptions" = return RuleDelete
parseTagDelete "MerchantGatewayAccountSubInfoOptions" = return MerchantGatewayAccountSubInfoDelete
parseTagDelete "MerchantCustomerCofDetailsOptions" = return MerchantCustomerCofDetailsDelete
parseTagDelete "OrderReferenceOptions" = return OrderReferenceDelete
parseTagDelete "MandateOptions" = return MandateDelete
parseTagDelete "GatewayTxnDataOptions" = return GatewayTxnDataDelete
parseTagDelete "MerchantGatewayCardInfoOptions" = return MerchantGatewayCardInfoDelete
parseTagDelete "TxnRiskCheckOptions" = return TxnRiskCheckDelete
parseTagDelete "PaymentMethodOptions" = return PaymentMethodDelete
parseTagDelete "OfferBenefitInfoOptions" = return OfferBenefitInfoDelete
parseTagDelete "MerchantIframePreferencesOptions" = return MerchantIframePreferencesDelete
parseTagDelete "ResellerAccountOptions" = return ResellerAccountDelete
parseTagDelete "UnifiedGatewayResponseOptions" = return UnifiedGatewayResponseDelete
parseTagDelete "WalletTopUpTxnOptions" = return WalletTopUpTxnDelete
parseTagDelete "IsinRoutesOptions" = return IsinRoutesDelete
parseTagDelete "TxnCardInfoOptions" = return TxnCardInfoDelete
parseTagDelete "OrderAddressOptions" = return OrderAddressDelete
parseTagDelete "LockerAccountOptions" = return LockerAccountDelete
parseTagDelete "JuspayBankCodeOptions" = return JuspayBankCodeDelete
parseTagDelete "MerchantPriorityLogicOptions" = return MerchantPriorityLogicDelete
parseTagDelete "IssuerRoutesOptions" = return IssuerRoutesDelete
parseTagDelete "RefundOptions" = return RefundDelete
parseTagDelete "SecondFactorOptions" = return SecondFactorDelete
parseTagDelete "GatewayOutageOptions" = return GatewayOutageDelete
parseTagDelete "TxnDetailOptions" = return TxnDetailDelete
parseTagDelete "EmiPlanOptions" = return EmiPlanDelete
parseTagDelete "MerchantKeyOptions" = return MerchantKeyDelete
parseTagDelete "NetworkCardFingerprintOptions" = return NetworkCardFingerprintDelete
parseTagDelete "TokenRequestorOptions" = return TokenRequestorDelete
parseTagDelete "TxnOfferDetailOptions" = return TxnOfferDetailDelete
parseTagDelete "SecondFactorResponseOptions" = return SecondFactorResponseDelete
parseTagDelete "EncryptionKeysOptions" = return EncryptionKeysDelete
parseTagDelete "OfferRedemptionOptions" = return OfferRedemptionDelete
parseTagDelete "CardBrandRoutesOptions" = return CardBrandRoutesDelete
parseTagDelete "CustomerOptions" = return CustomerDelete
parseTagDelete "MerchantAccountOptions" = return MerchantAccountDelete
parseTagDelete "ExternalMerchantCustomerOptions" = return ExternalMerchantCustomerDelete
parseTagDelete "TokenBinInfoOptions" = return TokenBinInfoDelete
parseTagDelete "MerchantGatewayPaymentMethodOptions" = return MerchantGatewayPaymentMethodDelete
parseTagDelete "PromotionOptions" = return PromotionDelete
parseTagDelete "LockerTokenRequestorOptions" = return LockerTokenRequestorDelete
parseTagDelete "BankAccountOptions" = return BankAccountDelete
parseTagDelete "AgencyOptions" = return AgencyDelete
parseTagDelete "ProviderOptions" = return ProviderDelete
parseTagDelete "GatewayCardInfoOptions" = return GatewayCardInfoDelete
parseTagDelete "PaymentGatewayResponseOptions" = return PaymentGatewayResponseDelete
parseTagDelete "MetadataOptions" = return MetadataDelete
parseTagDelete "GatewayTxnStatusMapOptions" = return GatewayTxnStatusMapDelete
parseTagDelete "JuspayErrorMapOptions" = return JuspayErrorMapDelete
parseTagDelete "ChargebackOptions" = return ChargebackDelete
parseTagDelete "ProcessTrackerOptions" = return ProcessTrackerDelete
parseTagDelete "ConfigurationsOptions" = return ConfigurationsDelete
parseTagDelete "WalletAccountOptions" = return WalletAccountDelete
parseTagDelete "GatewayStatusMapOptions" = return GatewayStatusMapDelete
parseTagDelete "TokenOptions" = return TokenDelete
parseTagDelete "MerchantLockerAccountOptions" = return MerchantLockerAccountDelete
parseTagDelete "JuspayEventOptions" = return JuspayEventDelete
parseTagDelete "TempCardOptions" = return TempCardDelete
parseTagDelete "MerchantRiskSettingsOptions" = return MerchantRiskSettingsDelete
parseTagDelete "UserOptions" = return UserDelete
parseTagDelete "CofDetailsOptions" = return CofDetailsDelete
parseTagDelete "JuspayIssuerNameMappingOptions" = return JuspayIssuerNameMappingDelete
parseTagDelete "ServiceConfigurationOptions" = return ServiceConfigurationDelete
parseTagDelete "OrderMetadataV2Options" = return OrderMetadataV2Delete
parseTagDelete "StoredCardOptions" = return StoredCardDelete
parseTagDelete "TokenCustomerOptions" = return TokenCustomerDelete
parseTagDelete "EnrolledPanOptions" = return EnrolledPanDelete
parseTagDelete "OffersOptions" = return OffersDelete
parseTagDelete "PaymentLinksOptions" = return PaymentLinksDelete
parseTagDelete "RoleOptions" = return RoleDelete
parseTagDelete "FeatureOptions" = return FeatureDelete
parseTagDelete "GatewayBankEmiSupportOptions" = return GatewayBankEmiSupportDelete
parseTagDelete "AuthenticationAccountOptions" = return AuthenticationAccountDelete
parseTagDelete "PaymentGatewayResponseV1Options" = return PaymentGatewayResponseV1Delete
parseTagDelete "SavedPaymentMethodOptions" = return SavedPaymentMethodDelete
parseTagDelete "MerchantProviderDetailsOptions" = return MerchantProviderDetailsDelete
parseTagDelete "TxnOfferOptions" = return TxnOfferDelete
parseTagDelete "GatewayHealthOptions" = return GatewayHealthDelete
parseTagDelete "RiskManagementAccountOptions" = return RiskManagementAccountDelete
parseTagDelete "CardInfoOptions" = return CardInfoDelete
parseTagDelete "DeviceBindingOptions" = return DeviceBindingDelete
parseTagDelete "NotificationOptions" = return NotificationDelete
parseTagDelete "OrderBasketOptions" = return OrderBasketDelete
parseTagDelete "GatewayPaymentMethodOptions" = return GatewayPaymentMethodDelete
parseTagDelete "CustomerAccountOptions" = return CustomerAccountDelete
parseTagDelete "EntityMapOptions" = return EntityMapDelete
parseTagDelete "FormInputOptions" = return FormInputDelete
parseTagDelete "IngressRuleOptions" = return IngressRuleDelete
parseTagDelete "InstallmentOptions" = return InstallmentDelete
parseTagDelete "InstallmentRefundOptions" = return InstallmentRefundDelete
parseTagDelete "PaymentFormOptions" = return PaymentFormDelete
parseTagDelete "UserRoleOptions" = return UserRoleDelete
parseTagDelete "TxnIntentDetailOptions" = return TxnIntentDetailDelete
parseTagDelete "AuthMappingOptions" = return AuthMappingDelete
parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

data DBDeleteObject
  = TxnOfferInfoDeleteOptions DeleteModel (Where MySQL TxnOfferInfoT)
  | HdfcHashedNumDeleteOptions DeleteModel (Where MySQL HdfcHashedNumT)
  | MerchantGatewayAccountDeleteOptions DeleteModel (Where MySQL MerchantGatewayAccountT)
  | MerchantGatewayAccountSubInfoDeleteOptions DeleteModel (Where MySQL MerchantGatewayAccountSubInfoT)
  | OrderReferenceDeleteOptions DeleteModel (Where MySQL OrderReferenceT)
  | MandateDeleteOptions DeleteModel (Where MySQL MandateT)
  | GatewayTxnDataDeleteOptions DeleteModel (Where MySQL GatewayTxnDataT)
  | MerchantGatewayCardInfoDeleteOptions DeleteModel (Where MySQL MerchantGatewayCardInfoT)
  | TxnRiskCheckDeleteOptions DeleteModel (Where MySQL TxnRiskCheckT)
  | PaymentMethodDeleteOptions DeleteModel (Where MySQL PaymentMethodT)
  | OfferBenefitInfoDeleteOptions DeleteModel (Where MySQL OfferBenefitInfoT)
  | MerchantIframePreferencesDeleteOptions DeleteModel (Where MySQL MerchantIframePreferencesT)
  | ResellerAccountDeleteOptions DeleteModel (Where MySQL ResellerAccountT)
  | UnifiedGatewayResponseDeleteOptions DeleteModel (Where MySQL UnifiedGatewayResponseT)
  | WalletTopUpTxnDeleteOptions DeleteModel (Where MySQL WalletTopUpTxnT)
  | IsinRoutesDeleteOptions DeleteModel (Where MySQL IsinRoutesT)
  | TxnCardInfoDeleteOptions DeleteModel (Where MySQL TxnCardInfoT)
  | OrderAddressDeleteOptions DeleteModel (Where MySQL OrderAddressT)
  | LockerAccountDeleteOptions DeleteModel (Where MySQL LockerAccountT)
  | JuspayBankCodeDeleteOptions DeleteModel (Where MySQL JuspayBankCodeT)
  | IssuerRoutesDeleteOptions DeleteModel (Where MySQL IssuerRoutesT)
  | RefundDeleteOptions DeleteModel (Where MySQL RefundT)
  | SecondFactorDeleteOptions DeleteModel (Where MySQL SecondFactorT)
  | GatewayOutageDeleteOptions DeleteModel (Where MySQL GatewayOutageT)
  | TxnDetailDeleteOptions DeleteModel (Where MySQL TxnDetailT)
  | EmiPlanDeleteOptions DeleteModel (Where MySQL EmiPlanT)
  | MerchantKeyDeleteOptions DeleteModel (Where MySQL MerchantKeyT)
  | NetworkCardFingerprintDeleteOptions DeleteModel (Where MySQL NetworkCardFingerprintT)
  | TokenRequestorDeleteOptions DeleteModel (Where MySQL TokenRequestorT)
  | TxnOfferDetailDeleteOptions DeleteModel (Where MySQL TxnOfferDetailT)
  | SecondFactorResponseDeleteOptions DeleteModel (Where MySQL SecondFactorResponseT)
  | CardBrandRoutesDeleteOptions DeleteModel (Where MySQL CardBrandRoutesT)
  | CustomerDeleteOptions DeleteModel (Where MySQL CustomerT)
  | MerchantAccountDeleteOptions DeleteModel (Where MySQL MerchantAccountT)
  | TokenBinInfoDeleteOptions DeleteModel (Where MySQL TokenBinInfoT)
  | MerchantGatewayPaymentMethodDeleteOptions DeleteModel (Where MySQL MerchantGatewayPaymentMethodT)
  | PromotionDeleteOptions DeleteModel (Where MySQL PromotionT)
  | LockerTokenRequestorDeleteOptions DeleteModel (Where MySQL LockerTokenRequestorT)
  | BankAccountDeleteOptions DeleteModel (Where MySQL BankAccountT)
  | ProviderDeleteOptions DeleteModel (Where MySQL ProviderT)
  | GatewayCardInfoDeleteOptions DeleteModel (Where MySQL GatewayCardInfoT)
  | PaymentGatewayResponseDeleteOptions DeleteModel (Where MySQL PaymentGatewayResponseT)
  | MetadataDeleteOptions DeleteModel (Where MySQL MetadataT)
  | ChargebackDeleteOptions DeleteModel (Where MySQL ChargebackT)
  | WalletAccountDeleteOptions DeleteModel (Where MySQL WalletAccountT)
  | GatewayStatusMapDeleteOptions DeleteModel (Where MySQL GatewayStatusMapT)
  | TokenDeleteOptions DeleteModel (Where MySQL TokenT)
  | MerchantLockerAccountDeleteOptions DeleteModel (Where MySQL MerchantLockerAccountT)
  | JuspayEventDeleteOptions DeleteModel (Where MySQL JuspayEventT)
  | TempCardDeleteOptions DeleteModel (Where MySQL TempCardT)
  | MerchantRiskSettingsDeleteOptions DeleteModel (Where MySQL MerchantRiskSettingsT)
  | UserDeleteOptions DeleteModel (Where MySQL UserT)
  | CofDetailsDeleteOptions DeleteModel (Where MySQL CofDetailsT)
  | OrderMetadataV2DeleteOptions DeleteModel (Where MySQL OrderMetadataV2T)
  | StoredCardDeleteOptions DeleteModel (Where MySQL StoredCardT)
  | TokenCustomerDeleteOptions DeleteModel (Where MySQL TokenCustomerT)
  | EnrolledPanDeleteOptions DeleteModel (Where MySQL EnrolledPanT)
  | RoleDeleteOptions DeleteModel (Where MySQL RoleT)
  | FeatureDeleteOptions DeleteModel (Where MySQL FeatureT)
  | GatewayBankEmiSupportDeleteOptions DeleteModel (Where MySQL GatewayBankEmiSupportT)
  | AuthenticationAccountDeleteOptions DeleteModel (Where MySQL AuthenticationAccountT)
  | PaymentGatewayResponseV1DeleteOptions DeleteModel (Where MySQL PaymentGatewayResponseV1T)
  | MerchantProviderDetailsDeleteOptions DeleteModel (Where MySQL MerchantProviderDetailsT)
  | TxnOfferDeleteOptions DeleteModel (Where MySQL TxnOfferT)
  | GatewayHealthDeleteOptions DeleteModel (Where MySQL GatewayHealthT)
  | RiskManagementAccountDeleteOptions DeleteModel (Where MySQL RiskManagementAccountT)
  | CardInfoDeleteOptions DeleteModel (Where MySQL CardInfoT)
  | DeviceBindingDeleteOptions DeleteModel (Where MySQL DeviceBindingT)
  | NotificationDeleteOptions DeleteModel (Where MySQL NotificationT)
  | OrderBasketDeleteOptions DeleteModel (Where MySQL OrderBasketT)
  | GatewayPaymentMethodDeleteOptions DeleteModel (Where MySQL GatewayPaymentMethodT)
  | PaymentLinksDeleteOptions DeleteModel (Where MySQL PaymentLinksT)
  | CustomerAccountDeleteOptions DeleteModel (Where MySQL CustomerAccountT)
  | EntityMapDeleteOptions DeleteModel (Where MySQL EntityMapT)
  | FormInputDeleteOptions DeleteModel (Where MySQL FormInputT)
  | IngressRuleDeleteOptions DeleteModel (Where MySQL IngressRuleT)
  | InstallmentDeleteOptions DeleteModel (Where MySQL InstallmentT)
  | InstallmentRefundDeleteOptions DeleteModel (Where MySQL InstallmentRefundT)
  | PaymentFormDeleteOptions DeleteModel (Where MySQL PaymentFormT)
  | UserRoleDeleteOptions DeleteModel (Where MySQL UserRoleT)
  | TxnIntentDetailDeleteOptions DeleteModel (Where MySQL TxnIntentDetailT)
  | AuthMappingDeleteOptions DeleteModel (Where MySQL AuthMappingT)
  | RuleDeleteOptions DeleteModel (Where Postgres RuleT)
  | OfferRedemptionDeleteOptions DeleteModel (Where Postgres OfferRedemptionT)
  | AgencyDeleteOptions DeleteModel (Where Postgres AgencyT)
  | ExternalMerchantCustomerDeleteOptions DeleteModel (Where Postgres ExternalMerchantCustomerT)
  | ProcessTrackerDeleteOptions DeleteModel (Where Postgres ProcessTrackerT)
  | OffersDeleteOptions DeleteModel (Where Postgres OffersT)
  | SavedPaymentMethodDeleteOptions DeleteModel (Where Postgres SavedPaymentMethodT)

instance ToJSON DBDeleteObject where
  toJSON = error "ToJSON not implemented for DBDeleteObject - Use getDbDeleteCommandJson instead" -- Using getDbDeleteCommandJson instead of toJSON

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    contents <- o .: "contents"
    deleteModel <- parseTagDelete =<< (o .: "tag")
    case deleteModel of
      TxnOfferInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnOfferInfoDeleteOptions deleteModel whereClause
      HdfcHashedNumDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ HdfcHashedNumDeleteOptions deleteModel whereClause
      MerchantGatewayAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantGatewayAccountDeleteOptions deleteModel whereClause
      RuleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RuleDeleteOptions deleteModel whereClause
      MerchantGatewayAccountSubInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantGatewayAccountSubInfoDeleteOptions deleteModel whereClause
      OrderReferenceDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OrderReferenceDeleteOptions deleteModel whereClause
      MandateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MandateDeleteOptions deleteModel whereClause
      GatewayTxnDataDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayTxnDataDeleteOptions deleteModel whereClause
      MerchantGatewayCardInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantGatewayCardInfoDeleteOptions deleteModel whereClause
      TxnRiskCheckDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnRiskCheckDeleteOptions deleteModel whereClause
      PaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentMethodDeleteOptions deleteModel whereClause
      OfferBenefitInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OfferBenefitInfoDeleteOptions deleteModel whereClause
      MerchantIframePreferencesDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantIframePreferencesDeleteOptions deleteModel whereClause
      ResellerAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ResellerAccountDeleteOptions deleteModel whereClause
      UnifiedGatewayResponseDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ UnifiedGatewayResponseDeleteOptions deleteModel whereClause
      WalletTopUpTxnDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ WalletTopUpTxnDeleteOptions deleteModel whereClause
      IsinRoutesDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IsinRoutesDeleteOptions deleteModel whereClause
      TxnCardInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnCardInfoDeleteOptions deleteModel whereClause
      OrderAddressDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OrderAddressDeleteOptions deleteModel whereClause
      LockerAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LockerAccountDeleteOptions deleteModel whereClause
      JuspayBankCodeDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ JuspayBankCodeDeleteOptions deleteModel whereClause
      IssuerRoutesDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssuerRoutesDeleteOptions deleteModel whereClause
      RefundDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RefundDeleteOptions deleteModel whereClause
      SecondFactorDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SecondFactorDeleteOptions deleteModel whereClause
      GatewayOutageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayOutageDeleteOptions deleteModel whereClause
      TxnDetailDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnDetailDeleteOptions deleteModel whereClause
      EmiPlanDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EmiPlanDeleteOptions deleteModel whereClause
      MerchantKeyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantKeyDeleteOptions deleteModel whereClause
      NetworkCardFingerprintDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ NetworkCardFingerprintDeleteOptions deleteModel whereClause
      TokenRequestorDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TokenRequestorDeleteOptions deleteModel whereClause
      TxnOfferDetailDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnOfferDetailDeleteOptions deleteModel whereClause
      SecondFactorResponseDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SecondFactorResponseDeleteOptions deleteModel whereClause
      OfferRedemptionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OfferRedemptionDeleteOptions deleteModel whereClause
      CardBrandRoutesDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CardBrandRoutesDeleteOptions deleteModel whereClause
      CustomerDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CustomerDeleteOptions deleteModel whereClause
      MerchantAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantAccountDeleteOptions deleteModel whereClause
      ExternalMerchantCustomerDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ExternalMerchantCustomerDeleteOptions deleteModel whereClause
      TokenBinInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TokenBinInfoDeleteOptions deleteModel whereClause
      MerchantGatewayPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantGatewayPaymentMethodDeleteOptions deleteModel whereClause
      PromotionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PromotionDeleteOptions deleteModel whereClause
      LockerTokenRequestorDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LockerTokenRequestorDeleteOptions deleteModel whereClause
      BankAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BankAccountDeleteOptions deleteModel whereClause
      AgencyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AgencyDeleteOptions deleteModel whereClause
      ProviderDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ProviderDeleteOptions deleteModel whereClause
      GatewayCardInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayCardInfoDeleteOptions deleteModel whereClause
      PaymentGatewayResponseDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentGatewayResponseDeleteOptions deleteModel whereClause
      MetadataDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MetadataDeleteOptions deleteModel whereClause
      ChargebackDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ChargebackDeleteOptions deleteModel whereClause
      ProcessTrackerDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ProcessTrackerDeleteOptions deleteModel whereClause
      WalletAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ WalletAccountDeleteOptions deleteModel whereClause
      GatewayStatusMapDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayStatusMapDeleteOptions deleteModel whereClause
      TokenDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TokenDeleteOptions deleteModel whereClause
      MerchantLockerAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantLockerAccountDeleteOptions deleteModel whereClause
      JuspayEventDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ JuspayEventDeleteOptions deleteModel whereClause
      TempCardDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TempCardDeleteOptions deleteModel whereClause
      MerchantRiskSettingsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantRiskSettingsDeleteOptions deleteModel whereClause
      UserDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ UserDeleteOptions deleteModel whereClause
      CofDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CofDetailsDeleteOptions deleteModel whereClause
      OrderMetadataV2Delete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OrderMetadataV2DeleteOptions deleteModel whereClause
      StoredCardDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ StoredCardDeleteOptions deleteModel whereClause
      TokenCustomerDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TokenCustomerDeleteOptions deleteModel whereClause
      EnrolledPanDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EnrolledPanDeleteOptions deleteModel whereClause
      OffersDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OffersDeleteOptions deleteModel whereClause
      RoleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RoleDeleteOptions deleteModel whereClause
      FeatureDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeatureDeleteOptions deleteModel whereClause
      GatewayBankEmiSupportDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayBankEmiSupportDeleteOptions deleteModel whereClause
      AuthenticationAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AuthenticationAccountDeleteOptions deleteModel whereClause
      PaymentGatewayResponseV1Delete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentGatewayResponseV1DeleteOptions deleteModel whereClause
      SavedPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SavedPaymentMethodDeleteOptions deleteModel whereClause
      MerchantProviderDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantProviderDetailsDeleteOptions deleteModel whereClause
      TxnOfferDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnOfferDeleteOptions deleteModel whereClause
      GatewayHealthDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayHealthDeleteOptions deleteModel whereClause
      RiskManagementAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RiskManagementAccountDeleteOptions deleteModel whereClause
      CardInfoDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CardInfoDeleteOptions deleteModel whereClause
      DeviceBindingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DeviceBindingDeleteOptions deleteModel whereClause
      NotificationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ NotificationDeleteOptions deleteModel whereClause
      OrderBasketDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OrderBasketDeleteOptions deleteModel whereClause
      GatewayPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GatewayPaymentMethodDeleteOptions deleteModel whereClause
      PaymentLinksDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentLinksDeleteOptions deleteModel whereClause
      CustomerAccountDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CustomerAccountDeleteOptions deleteModel whereClause
      EntityMapDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EntityMapDeleteOptions deleteModel whereClause
      FormInputDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FormInputDeleteOptions deleteModel whereClause
      IngressRuleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IngressRuleDeleteOptions deleteModel whereClause
      InstallmentDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ InstallmentDeleteOptions deleteModel whereClause
      InstallmentRefundDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ InstallmentRefundDeleteOptions deleteModel whereClause
      PaymentFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentFormDeleteOptions deleteModel whereClause
      UserRoleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ UserRoleDeleteOptions deleteModel whereClause
      TxnIntentDetailDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TxnIntentDetailDeleteOptions deleteModel whereClause
      AuthMappingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AuthMappingDeleteOptions deleteModel whereClause
