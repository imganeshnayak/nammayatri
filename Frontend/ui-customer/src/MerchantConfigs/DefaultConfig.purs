module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig

config :: AppConfig 
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , estimateConfirmText: "Request a NammaYatri Ride"
  , autoConfirmingLoaderColor: "#80B2FF"
  , quoteListModelBackground: "#2C2F3A"
  , defaultLanguage : "EN_US"
  , currency: "₹"
  , primaryButtonCornerRadius: 8.0
  , showPickUpandDrop: true
  , alertDialogPrimaryColor: "#2194FF"
  , cancelSearchTextColor: "#E55454"
  , showHamMenu : true
  , showQuoteFindingText : false
  , quoteListItemConfig: 
    { primaryButtonCorner: 8.0
    , expiresColor: "#E55454"
    , driverImagebg: "#F1F1F1"
    , vehicleHeight: 37
    , vehicleWidth: 40
    }
  , quoteListModel:
    { backgroundColor: "#2C2F3A"
    , textColor: "#FFFFFF"
    , loaderColor: "#80B2FF"
    , otpTextBackground : "#2C2F3A"
    , otpBackground: "#F1F1F1"
    , otpTextColor: "#FFFFFF"
    , otpTitleColor : "#6D7280"
    , selectRideTextColor: "#2C2F3A"
    , lineImage : "ic_line"
    , lottieHeight : 300
    , lottieWidth : 300
    , topMargin : 0
    , noQuotesImageHeight: 115
    , noQuotesImageWidth : 137
    , separatorColor : "#00FFFFFF"
    , showSeparator : false
    , closeIcon : "ny_ic_close_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_close_white.png"
    }
  , searchLocationConfig : 
    { searchLocationTheme: "#2C2F3A"
    , setLocationOnMapColor:"#6D7280"
    , editTextBackground : "#313440"
    , editTextDefaultColor : "#A7A7A7"
    , strokeColor: "1,#E5E7EB"
    , backgroundColor : "#2C2F3A"
    , editTextColor : "#FFFFFF"
    , separatorColor : "#00FFFFFF"
    , enableLocationTagbar : "true"
    , resultsCardCornerRadius : 20.0
    , showRateCardDetails : true
    , showAdditionalChargesText : false
    , lottieHeight : 96
    , lottieWidth : 96
    , primaryButtonHeight : 60
    , hintColor : "#A7A7A7"
    , showSeparator : false
    , showChargeDesc: false
    , backArrow : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
    , enableRateCard: true
    }
  , driverInfoConfig : 
    { ratingTextColor: "#454545"
    , ratingBackground: "#F1F1F1"
    , ratingStroke: "0,#717171"
    , ratingCornerRadius: 6.0
    , callBackground: "#2053BB6F"
    , callButtonStroke: "0,#EB0055" 
    , cardStroke: "1,#E5E7EB"
    , otpStroke: "0,#717171"
    , showNumberPlatePrefix : true
    , showNumberPlateSuffix : false
    , callHeight: 24
    , callWidth: 24
    , numberPlateBackground : "#E9BE4D"
    , showCancelPrevention : true
    , showTrackingButton : true
    , specialZoneQuoteExpirySeconds : 3600
    , footerVisibility : false
    , footerImageUrl : "ic_namma_yatri_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ic_namma_yatri_logo.png"
    , footerBackgroundColor : "#FFFFFF"
    }
  , ratingConfig : 
    { secondaryButtonTextColor : "#2C2F3A"
    , secondaryButtonStroke : "1,#2C2F3A"
    , buttonCornerRadius : 8.0
    }
  , cancelReasonConfig : 
    { secondaryButtonTextColor : "#2C2F3A"
    , secondaryButtonStroke : "1,#2C2F3A"
    , buttonCornerRadius : 8.0
    }
  , profileBackground: "#2C2F3A"
  , profileName: "#FFFFFF"
  , profileImage: "#012A72"
  , feedbackBackground: "#2C2F3A"
  , sideBarList: [ "MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout" ]
  , rateCardColor: "#2C2F3A"
  , nyBrandingVisibility: false
  , fontType: "Assets"
  , languageList : []
  , confirmPickUpLocationBorder: "#E5E7EB"
  , bannerConfig : {
        backgroundColor : "#F0FAF0"
      , title : "Complete your profile for a personalised ride experience"
      , titleColor :"#21C179"
      , actionText : "Update now"
      , actionTextColor : "#27AE5F"
      , imageUrl : "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png" 
      }
  , popupBackground : "#FFFFFF"
  , profileCompletion : "#FCC32C"
  , showProfileStatus: true
  , profileArrowImage: "ny_ic_chevron_right_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_right_white.png"
  , cancelRideColor : "#E55454"
  , infoIconUrl : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
  , profileEditGravity : "center"
  , merchantLogo : "ic_launcher,https://assets.juspay.in/nammayatri/images/user/ny_ic_launcher.png"
  , logs: [ "JUSPAY" ]
  , showCorporateAddress : true
  , terminateBtnConfig : {
          visibility: false, 
          title : "",
          backgroundColor : "#00FFFFFF",
          imageUrl : "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
      }
  , suggestedTripsAndLocationConfig : {
        geohashLimitForMap : 60,
        geohashPrecision : 7,
        locationsToBeShown : 5,
        tripsToBeShown : 5,
        locationsToBeStored : 30,
        tripsToBeStored : 30,
        frequencyWeight : 0.7,
        tripDistanceThreshold : 0.021
      }
  , showDeleteAccount : false
  , autoSelectBackground : "#53BB6F"
  , showGenderBanner : true
  , enableMockLocation : false
  , specialLocationView : false
  , internationalNumberEnabled : false
  , callOptions : ["ANONYMOUS"]
  , autoVariantEnabled : true
  , showDisabilityBanner : false
  , geoCoder: {
      enableLLtoAddress : true
    , enableAddressToLL : true 
    }
  , enableWhatsappOTP : ["BD"]
  , notifyRideConfirmationConfig : 
      { notify : false 
      , autoGeneratedText : ""
      }
  , estimateAndQuoteConfig : 
      { variantTypes : [ ["SUV"], ["HATCHBACK", "TAXI_PLUS", "SEDAN"], ["TAXI"], ["AUTO_RICKSHAW"] ]
      , variantOrder : ["HATCHBACK", "TAXI_PLUS", "SEDAN", "TAXI", "SUV", "AUTO_RICKSHAW"]
      , enableOnlyAuto : false
      , showNearByDrivers: false
      , enableBookingPreference: true
      }
  , customerTip : {
      auto : true,
      cabs : false
    }
  , feature : {
    enableAutoReadOtp : true,
    enableZooTicketBookingFlow : false,
    enableShareRide : true,
    enableChat: true,
    enableEmergencyContacts: true,
    enableReferral: true,
    enableSupport: true,
    enableShareApp: true
  }

  , rideCompletedCardConfig : {
      topCard : {
        gradient : "#29334A"
      , enableGradient : true
      , background : "#2C2F3A"
      } 
  }
  , mapConfig : 
      { locateOnMapConfig : 
          { dottedLineConfig : 
              { visible : false
              , range : 100
              , color : "#323643"
              },
            apiTriggerRadius : 10.0,
            pickUpToSourceThreshold : 1.0
          }
      , labelTextSize : 30
      , animationDuration : 500
      , vehicleMarkerSize: 90
      }
  , purpleRideConfig : {
      genericVideoUrl : "" ,
      visualImpairmentVideo : "" ,
      physicalImpairmentVideo : "",
      hearingImpairmentVideo : ""
    }
  , homeScreen: {
      primaryBackground : "#2C2F3A",
      pickUpViewColor : "#303440",
      header : {
        menuButtonBackground : "#00FFFFFF",
        showLogo : true,
        titleColor : "#1D1D1D",
        showSeparator : false
      },
      bannerViewVisibility : true,
      pickupLocationTextColor : "#A7A7A7",
      whereToButton : {
        margin : {
          top : 0,
          left : 16,
          right : 16,
          bottom : 16
        },
        shadow : {
          color : "#2C2F3A",
          x: 0.0,
          y: 0.9,
          blur: 10.0,
          spread: 24.0,
          opacity : 0.14
        }
      }
    }
  , appData : defaultAppData
  , navigationAppConfig : defaultNavigationAppConfig
  , genericHeaderConfig : defaultGenericHeader
  , colors : defaultColors
  , primaryButtonConfig : defaultPrimaryButtonConfig
  , fontConfig : defaultFontConfig
  , loaderConfig : defaultLoaderConfig
  , otpRegex :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , termsLink : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA"
  , privacyLink : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
  , shareAppConfig : {
      title : "Share Namma Yatri!"
    , description : "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen"
  }
  , dashboard :{
      url : "https://nammayatri.in/open?source=in-app"
    , enable : false
  }
}
