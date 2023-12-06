{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Screens.Types
import Prelude(negate)
import Services.API (DriverProfileStatsResp(..), Status(..))
import Data.Maybe
import Foreign.Object (empty)
import Common.Types.App as Common
import MerchantConfig.DefaultConfig as DC

initData :: HomeScreenState
initData = {
    data: {
        snappedOrigin : Nothing,
        config : DC.config,
        driverName : "",
        vehicleType : "",
        profileImg : Nothing,
        driverAlternateMobile : Nothing,
        activeRide : {
          id : "",
          source : "",
          destination : "",
          src_lat : 0.0,
          src_lon : 0.0,
          dest_lat : 0.0,
          dest_lon : 0.0,
          actualRideDistance : 0.0,
          status : NOTHING,
          distance : 0.0,
          duration : 0,
          riderName : "",
          estimatedFare : 0,
          waitTimerId : "",
          notifiedCustomer : false,
          exoPhone : "",
          specialLocationTag : Nothing,
          waitTimeSeconds : -1,
          waitTimeInfo : false,
          rideCreatedAt : "",
          requestedVehicleVariant : Nothing,
          disabilityTag : Nothing
        },
        cancelRideModal : {
          selectionOptions : [],
          activeIndex : Nothing,
          selectedReasonCode : "",
          selectedReasonDescription : "",
          isMandatoryTextHidden : false,
          isSelectButtonActive : false
        },
        currentDriverLat : 0.0,
        currentDriverLon : 0.0,
        locationLastUpdatedTime : "",
        totalRidesOfDay : 2,
        totalEarningsOfDay : 2,
        bonusEarned : 0,
        route : [],
        cancelRideConfirmationPopUp : {
          delayInSeconds : 5,
          timerID : "",
          continueEnabled : false,
          enableTimer : true
        },
        messages : [],
        messagesSize : "-1",
        suggestionsList : [],
        messageToBeSent : "",
        logField : empty, 
        paymentState : {
          rideCount : 0,
          totalMoneyCollected : 0,
          payableAndGST : 0,
          platFromFee : 0,
          date : "",
          dateObj : "",
          makePaymentModal : false,
          showRateCard : false,
          paymentStatusBanner : false,
          paymentStatus : Common.Success,
          invoiceId : "",
          bannerBG : "",
          bannerTitle : "",
          bannerTitleColor : "",
          banneActionText : "",
          actionTextColor : "",
          totalPendingManualDues : 0.0,
          bannerImage : "",
          showBannerImage : false,
          chargesBreakup : [],
          blockedDueToPayment : false,
          laterButtonVisibility : false,
          orderId : "",   
          subscribed : true,
          showShimmer : false,
          driverBlocked : false,
          showBlockingPopup : false,
          autoPayStatus : NO_AUTOPAY
        },
        triggerPatchCounter : 0,
        peekHeight : 0,
        endRideData : {
          rideId : "",
          zeroCommision : 0,
          tip : Nothing,
          finalAmount : 0, 
          riderName : "",
          rating : 0,
          feedback : "",
          disability : Nothing,
          payerVpa : ""
        },
        driverGotoState : {
          gotoCount : 0,
          goToInfo : false,
          selectedGoTo : "",
          savedLocationsArray : [],
          showGoto : false,
          gotoValidTill : "-",
          timerInMinutes : "-",
          isGotoEnabled : false,
          timerId : "",
          gotoReducedCount : Nothing,
          gotoLocInRange : false,
          goToPopUpType : NO_POPUP_VIEW,
          gotoEnabledForMerchant : false,
          confirmGotoCancel : false
        }
    },
    props: {
        statusOnline : true,
        driverStatusSet : Online,
        goOfflineModal : false,
        screenName : "Home",
        rideActionModal : false,
        updatedArrivalInChat : false,
        enterOtpModal : false,
        rideOtp : "",
        enterOtpFocusIndex : 0,
        time : 0,
        otpIncorrect : false,
        wrongVehicleVariant : false,
        endRidePopUp : false,
        cancelRideModalShow : false,
        routeVisible : false,
        otpAttemptsExceeded : false,
        refreshAnimation : false,
        showDottedRoute : false,
        currentStage : HomeScreen,
        mapRendered : false,
        cancelConfirmationPopup : false,
        chatcallbackInitiated : false,
        sendMessageActive : false,
        unReadMessages : false,
        openChatScreen : false,
        silentPopUpView : false,
        zoneRideBooking : true,
        showGenderBanner : false,
        notRemoveBanner : true,
        showBonusInfo : false,
        showlinkAadhaarPopup : false,
        isChatOpened : false,
        showAadharPopUp : true,
        canSendSuggestion : true,
        showOffer : false,
        rcActive : false, 
        rcDeactivePopup : false,
        autoPayBanner : NO_SUBSCRIPTION_BANNER,
        showAccessbilityPopup : false,
        showRideCompleted : false,
        showRideRating : false,
        showContactSupportPopUp : false,
        showChatBlockerPopUp : false,
        subscriptionPopupType : NO_SUBSCRIPTION_POPUP,
        showGenericAccessibilityPopUp : false,
        waitTimeStatus : NoStatus,
        isMockLocation : false,
        accountBlockedPopup : false
    }
}

dummyDriverRideStats :: DriverProfileStatsResp
dummyDriverRideStats = DriverProfileStatsResp
    {
      totalRidesOfDay : 0
    , totalEarningsOfDay : 0
    , bonusEarning : 0
    }