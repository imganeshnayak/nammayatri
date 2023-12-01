{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Engineering.Helpers.LogEvent
import Accessor (_computedPrice, _contents, _formattedAddress, _id, _lat, _lon, _status, _toLocation, _signatureAuthData, _payload, _view_param, _show_splash, _middleName, _firstName, _lastName)
import Common.Types.App (GlobalPayload(..), SignatureAuthData(..), Payload(..), Version(..), LocationData(..), EventPayload(..), ClevertapEventParams, OTPChannel(..), LazyCheck(..), FCMBundleUpdate)
import Components.LocationListItem.Controller (dummyLocationListState)
import Components.SavedLocationCard.Controller (getCardType)
import Components.SettingSideBar.Controller as SettingSideBarController
import Constants as Constants
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Data.Array (catMaybes, filter, length, null, snoc, (!!), any, sortBy, head, uncons, last)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn3, runFn2, runFn1)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (Pattern(..), drop, indexOf, split, toLower, trim, take, joinWith)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, getExpiryTime, convertUTCtoISC, getCurrentUTC, getWindowVariable, flowRunner)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (suggestionsDefinitions, getSuggestions)
import Engineering.Helpers.Utils (loaderText, toggleLoader, getAppConfig, saveObject, reboot, showSplash, catMaybeStrings)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Encode, encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import JBridge (addMarker, cleverTapSetLocation, currentPosition, drawRoute, emitJOSEvent, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, firebaseUserID, generateSessionId, getLocationPermissionStatus, getVersionCode, getVersionName, hideKeyboardOnNavigation, hideLoader, initiateLocationServiceClient, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, launchInAppRatingPopup, locateOnMap, locateOnMapConfig, metaLogEvent, openNavigation, reallocateMapFragment, removeAllPolylines, saveSuggestionDefs, saveSuggestions, setCleverTapUserData, setCleverTapUserProp, stopChatListenerService, toast, toggleBtnLoader, updateRoute, updateRouteMarker, extractReferrerUrl, getLocationNameV2, getLatLonFromAddress)
import Helpers.Utils (decodeError, addToPrevCurrLoc, addToRecentSearches, adjustViewWithKeyboard, checkPrediction, clearWaitingTimer, differenceOfLocationLists, drawPolygon, filterRecentSearches, fetchImage, FetchImageFrom(..), getCurrentDate, getNextDateV2, getCurrentLocationMarker, getCurrentLocationsObjFromLocal, getDistanceBwCordinates, getGlobalPayload, getMobileNumber, getNewTrackingId, getObjFromLocal, getPrediction, getRecentSearches, getScreenFromStage, getSearchType, parseFloat, parseNewContacts, removeLabelFromMarker, requestKeyboardShow, saveCurrentLocations, seperateByWhiteSpaces, setText, showCarouselScreen, sortPredctionByDistance, toStringJSON, triggerRideStatusEvent, withinTimeRange, fetchDefaultPickupPoint, recentDistance)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import MerchantConfig.Utils as MU
import ModifyScreenState (modifyScreenState, updateRideDetails)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, otherwise, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>), (<<<), ($>), (>>=), (*))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField, delay)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getSearchRadius, getValueByComponent, getWard, ticketPlaceId)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation, getLocTag) as AddNewAddress
import Screens.AddNewAddressScreen.ScreenData (dummyLocation) as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime, isTipEnabled, findingQuotesSearchExpired, tipEnabledState)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (getLocationList, getDriverInfo, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList, getSpecialTag, transformHotSpotInfo)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.TicketBookingScreen.ScreenData as TicketBookingScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.RideBookingFlow.HomeScreen.Config (getTipViewData, setTipViewData)
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationIcons, specialLocationConfig, updateRouteMarkerConfig)
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.Types (TicketBookingScreenStage(..), CardType(..), AddNewAddressScreenState(..), SearchResultType(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), ErrorType(..), ZoneType(..), TipViewData(..),TripDetailsGoBackType(..), Location, DisabilityT(..), UpdatePopupType(..) , PermissionScreenStage(..), TicketBookingItem(..), TicketBookings(..), TicketBookingScreenData(..),TicketInfoScreenData(..),IndividualBookingItem(..))
import Screens.Types (Gender(..)) as Gender
import Services.API --(AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), SelectEstimateRes(..), UpdateProfileReq(..), OnCallRes(..), Snapped(..), AddressComponents(..), FareBreakupAPIEntity(..), GetDisabilityListResp(..), Disability(..), PersonStatsRes(..))
import Services.API --(AuthType(..), AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), TriggerSignatureOTPResp(..), User(..), OnCallRes(..)) --TODO:: Need to fix duplicate imports
import Services.Backend as Remote
import Services.Config (getBaseUrl)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), TICKET_BOOKING_SCREEN_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), APP_UPDATE_POPUP(..), TICKET_BOOKING_SCREEN_OUTPUT(..),TICKET_INFO_SCREEN_OUTPUT(..),defaultGlobalState)
import Effect.Aff (Milliseconds(..), makeAff, nonCanceler, launchAff)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)
import Constants.Configs
import PrestoDOM (initUI)
import Common.Resources.Constants (zoomLevel)
import PaymentPage
import Screens.TicketBookingScreen.Transformer
import Screens.Types as ST
import Common.Types.App as Common
import PrestoDOM.Core (terminateUI)
import Helpers.Storage.Flow.BaseApp
import Helpers.Logs -- TODO :: Move helpers import into a single file and reexport
import Helpers.Auth
import Helpers.Version
import Helpers.Ride
import Helpers.Firebase

baseAppFlow :: GlobalPayload -> Boolean-> FlowBT String Unit
baseAppFlow gPayload callInitUI = do
  baseAppStorage -- TODO:: Restructure the files and names
  baseAppLogs
  checkVersion
  let showSplashScreen = fromMaybe false $ gPayload ^. _payload ^. _show_splash
  when callInitUI $ lift $ lift $ initUI -- TODO:: Can we move this to Main
  when showSplashScreen $ toggleSplash true
  tokenValidity <- validateToken signatureAuthData
  if tokenValidity 
    then handleDeepLinks (Just gPayload) false
    else validateAuthData $ signatureAuthData
  
  where
    signatureAuthData = 
      gPayload ^._payload ^._signatureAuthData
    validateAuthData signatureAuthData = 
      case signatureAuthData of
        Just signatureAuth -> do
          response <- lift $ lift $ Remote.triggerSignatureBasedOTP signatureAuth
          validationStatus <- validateSignaturePayload signatureAuth response
          when validationStatus $ handleDeepLinks (Just gPayload) false
        Nothing -> 
          if showCarouselScreen FunctionCall
            then welcomeScreenFlow
            else enterMobileNumberScreenFlow

handleDeepLinks :: Maybe GlobalPayload -> Boolean -> FlowBT String Unit
handleDeepLinks mBGlobalPayload skipDefaultCase = do
  case mBGlobalPayload of 
    Just globalPayload ->
      case globalPayload ^. _payload ^._view_param of
        Just screen -> case screen of
          "rides" -> hideSplashAndCallFlow $ myRidesScreenFlow true
          "abt" -> hideSplashAndCallFlow aboutUsScreenFlow
          "fvrts" -> hideSplashAndCallFlow savedLocationFlow
          "help" -> hideSplashAndCallFlow helpAndSupportScreenFlow
          "prof" -> hideSplashAndCallFlow myProfileScreenFlow
          "lang" -> hideSplashAndCallFlow selectLanguageScreenFlow
          "tkts" -> hideSplashAndCallFlow zooTicketBookingFlow
          _ -> if skipDefaultCase then pure unit else currentFlowStatus
        Nothing -> currentFlowStatus
    Nothing -> do
      mBPayload <- liftFlowBT $ getGlobalPayload unit
      case mBPayload of
        Just _ -> handleDeepLinks mBPayload skipDefaultCase
        Nothing -> pure unit

hideSplashAndCallFlow :: FlowBT String Unit -> FlowBT String Unit
hideSplashAndCallFlow flow = do 
  hideLoaderFlow
  flow

hideLoaderFlow :: FlowBT String Unit
hideLoaderFlow = do
  toggleSplash false
  void $ lift $ lift $ toggleLoader false
  liftFlowBT $ hideLoader

toggleSplash :: Boolean -> FlowBT String Unit
toggleSplash = 
  if _ then UI.splashScreen
  else do
    state <- getState
    void $ liftFlowBT $ launchAff $ flowRunner state $ runExceptT $ runBackT $ do
      void $ lift $ lift $ delay $ Milliseconds 2000.0
      liftFlowBT $ terminateUI $ Just "SplashScreen"


appUpdatedFlow :: FCMBundleUpdate -> FlowBT String Unit
appUpdatedFlow payload = do
  modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppUpdated ,appUpdatedView{secondaryText=payload.description,primaryText=payload.title,coverImageUrl=payload.image}})
  fl <- UI.handleAppUpdatePopUp
  case fl of
    UpdateNow -> do 
      liftFlowBT showSplash
      liftFlowBT reboot
    Later -> pure unit

currentFlowStatus :: FlowBT String Unit
currentFlowStatus = do
  void $ lift $ lift $ toggleLoader false
  setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL" --TODO:: How is this being used @rohit??
  verifyProfile "LazyCheck"
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  case flowStatus.currentStatus of
    WAITING_FOR_DRIVER_OFFERS currentStatus -> goToFindingQuotesStage currentStatus.estimateId false
    DRIVER_OFFERED_QUOTE currentStatus      -> goToFindingQuotesStage currentStatus.estimateId true
    RIDE_ASSIGNED _                         -> checkRideStatus true
    _                                       -> checkRideStatus false
  hideLoaderFlow
  _ <- pure $ hideKeyboardOnNavigation true
  -- zooTicketBookingFlow
  homeScreenFlow
  where
    verifyProfile :: String -> FlowBT String Unit
    verifyProfile dummy = do
      (GetProfileRes response) <- Remote.getProfileBT ""
      updateVersion response.clientVersion response.bundleVersion
      updateFirebaseToken response.maskedDeviceToken getUpdateToken
      updateUserLanguage response.language
      let name = catMaybeStrings [response.firstName, response.middleName, response.lastName]
      void $ pure $ setCleverTapUserData "Name" name

      when (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") $ do
          case response.gender of
              Just value -> void $ pure $ setCleverTapUserData "gender" value
              Nothing -> pure unit

      case response.language of
          Just value -> void $ pure $ setCleverTapUserData "Preferred Language" value
          Nothing -> pure unit

      void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore CUSTOMER_ID)
      void $ pure $ setCleverTapUserData "Phone" ("+91" <> (getValueToLocalStore MOBILE_NUMBER))
      setValueToLocalStore DISABILITY_UPDATED $ if isNothing response.hasDisability then "false" else "true"
      case response.disability of
        Just disabilityType -> setValueToLocalStore DISABILITY_NAME disabilityType 
        Nothing -> pure unit
      setValueToLocalStore REFERRAL_STATUS  $ if response.hasTakenRide then "HAS_TAKEN_RIDE" else if (response.referralCode /= Nothing && not response.hasTakenRide) then "REFERRED_NOT_TAKEN_RIDE" else "NOT_REFERRED_NOT_TAKEN_RIDE"
      setValueToLocalStore HAS_TAKEN_FIRST_RIDE if response.hasTakenRide then "true" else "false"
      -- (PersonStatsRes resp) <- Remote.getPersonStatsBT "" -- TODO:: Make this function async in non critical flow @ashkriti
      if DS.null (fromMaybe "" response.firstName) && isNothing response.firstName then do
        void $ updateLocalStage HomeScreen
        hideLoaderFlow
        accountSetUpScreenFlow
        handleDeepLinks Nothing true
      else do
        let tag = case (response.disability) of
                      Just value -> value
                      Nothing -> ""
        modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{ disability = Just {tag : tag, id : "", description: ""}, settingSideBar{name =fromMaybe ""  response.firstName}}})
        setValueToLocalStore USER_NAME ((fromMaybe "" response.firstName) <> " " <> (fromMaybe "" response.middleName) <> " " <> (fromMaybe "" response.lastName))
      if (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just (fromMaybe "" response.gender)}} , props {isBanner = false}})
        else pure unit
      if isJust response.email then do
        setValueToLocalStore USER_EMAIL $ fromMaybe "" response.email
        case response.email of
            Just value -> void $ pure $ setCleverTapUserData "Email" value
            Nothing -> pure unit
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just (fromMaybe "" response.email)}}})
        else pure unit
    
    getUpdateToken :: String -> FlowBT String Unit
    getUpdateToken token =
      let
        UpdateProfileReq initialData = Remote.mkUpdateProfileRequest FunctionCall
        requiredData = initialData { deviceToken = Just token }
      in
        void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
    
    updateUserLanguage :: Maybe String -> FlowBT String Unit
    updateUserLanguage language = 
      when (isNothing language || (getKeyByLanguage (fromMaybe "ENGLISH" language) /= (getValueToLocalNativeStore LANGUAGE_KEY)))
        $ void $ lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)

    goToFindingQuotesStage :: String -> Boolean -> FlowBT String Unit
    goToFindingQuotesStage estimateId driverOfferedQuote = do
      if any (_ == (getValueToLocalStore FINDING_QUOTES_START_TIME)) ["__failed", ""] then do
        updateFlowStatus SEARCH_CANCELLED
      else do
        let searchExpiryTime = getSearchExpiryTime "LazyCheck"
            secondsLeft = findingQuotesSearchExpired driverOfferedQuote
        if secondsLeft > 0 then do
          setValueToLocalStore RATING_SKIPPED "true"
          updateLocalStage FindingQuotes
          setValueToLocalStore AUTO_SELECTING ""
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          (GlobalState currentState) <- getState
          let tipViewData = case (getTipViewData "LazyCheck") of
                              Just (TipViewData tipView) -> do
                                currentState.homeScreen.props.tipViewProps{stage = tipView.stage , activeIndex = tipView.activeIndex , isVisible = tipView.isVisible }
                              Nothing -> do
                                currentState.homeScreen.props.tipViewProps
          case (getFlowStatusData "LazyCheck") of
            Just (FlowStatusData flowStatusData) -> do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{
                props{ sourceLat = flowStatusData.source.lat
                     , sourceLong = flowStatusData.source.lng
                     , destinationLat = flowStatusData.destination.lat
                     , destinationLong = flowStatusData.destination.lng
                     , currentStage = FindingQuotes
                     , searchExpire = secondsLeft
                     , estimateId = estimateId
                     , rideRequestFlow = true
                     , selectedQuote = Nothing
                     , tipViewProps = tipViewData
                     , city = flowStatusData.source.city
                     , findingQuotesProgress = 1.0 - (INT.toNumber secondsLeft)/(INT.toNumber searchExpiryTime)}
                , data { source = flowStatusData.source.place
                       , destination = flowStatusData.destination.place
                       , sourceAddress = flowStatusData.sourceAddress
                       , destinationAddress = flowStatusData.destinationAddress }
                })
            Nothing -> updateFlowStatus SEARCH_CANCELLED
        else updateFlowStatus SEARCH_CANCELLED

-- TODO :: currently this flow is not in use
-- chooseLanguageScreenFlow :: FlowBT String Unit
-- chooseLanguageScreenFlow = do
--   logField_ <- lift $ lift $ getLogFields
--   hideLoaderFlow
--   setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage"
--   _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_choose_lang_scn_view"
--   flow <- UI.chooseLanguageScreen
--   case flow of
--     NextScreen language -> do
--                             setValueToLocalStore LANGUAGE_KEY language
--                             void $ pure $ setCleverTapUserProp [{key : "Preferred Language", value : unsafeToForeign language}]
--                             _ <- lift $ lift $ liftFlow $(logEventWithParams logField_ "ny_user_lang_choose" "language" (language))
--                             enterMobileNumberScreenFlow
--     Refresh state -> chooseLanguageScreenFlow

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  hideLoaderFlow -- Removed initial choose langauge screen
  if(getValueToLocalStore LANGUAGE_KEY == "__failed") then setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage" else pure unit
  if ( any (_ == getValueToLocalStore REGISTERATION_TOKEN) ["__failed", "(null)"]) && ( any (_ == getValueToLocalStore REFERRER_URL) ["__failed", "(null)"]) then do
    _ <- pure $ extractReferrerUrl unit
    pure unit
  else pure unit
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {data {config =  config }})
  logField_ <- lift $ lift $ getLogFields
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_enter_mob_num_scn_view"
  flow <- UI.enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString VERIFYING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            let generatedID = "generated_" <> (generateSessionId unit)
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp generatedID) state.data.tokenId
            case resp of
              Right resp -> do
                    _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_verify_otp"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    if (customerId == "__failed") then do
                      _ <- lift $ lift $ setLogField "customer_id" $ encode ("null")
                      pure unit
                      else do
                        _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
                        pure unit
                    setValueToLocalStore CUSTOMER_ID customerId
                    void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore CUSTOMER_ID)
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    setValueToLocalStore USER_NAME $ (fromMaybe "" $ response.person ^. _firstName) <> " " <> (fromMaybe "" $ response.person ^. _middleName) <> " " <> (fromMaybe "" $ response.person ^. _lastName)
                    if isNothing (response.person ^. _firstName) then currentFlowStatus else handleDeepLinks Nothing false
              Left err -> do
                pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
                let errResp = err.response
                    codeMessage = decodeError errResp.errorMessage "errorCode"
                if ( err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    _ <- pure $ toast (getString OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{data{otp=""}, props{enterOTP = false, wrongOTP = false}})
                else if ( err.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                    let attemptsLeft = decodeError errResp.errorMessage "errorPayload"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false, attemptLeft = attemptsLeft}, data{otp=""}})
                else if ( err.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
                    pure $ toast (getString TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false, wrongOTP = false}, data{otp=""}})
                else do
                    pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false,wrongOTP = false}, data{otp=""}})
                enterMobileNumberScreenFlow
    GoToOTP state -> do
            setValueToLocalStore MOBILE_NUMBER (state.data.mobileNumber)
            setValueToLocalStore COUNTRY_CODE (state.data.countryObj.countryCode)
            void $ pure $ setCleverTapUserData "Phone" (state.data.countryObj.countryCode <> (getValueToLocalStore MOBILE_NUMBER))
            (TriggerOTPResp triggerOtpResp) <- Remote.triggerOTPBT (Remote.makeTriggerOTPReq state.data.mobileNumber state.data.countryObj.countryCode (show state.data.otpChannel))
            _ <- pure $ toast (getString if state.data.otpChannel == SMS then SENT_OTP_VIA_SMS else  SENT_OTP_VIA_WHATSAPP) 
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = triggerOtpResp.authId, attempts = triggerOtpResp.attempts}, props {enterOTP = true,resendEnable = false}})
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{number = state.data.mobileNumber}}})
            enterMobileNumberScreenFlow
    ResendOTP state -> do
            (ResendOTPResp resendResp) <-  Remote.resendOTPBT state.data.tokenId
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = resendResp.authId, attempts = resendResp.attempts}})
            enterMobileNumberScreenFlow
    GoBack state  ->  do
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data {timer = 30 }, props {enterOTP = false,resendEnable = false}})
            enterMobileNumberScreenFlow
    GoToWelcomeScreen state -> welcomeScreenFlow

welcomeScreenFlow :: FlowBT String Unit
welcomeScreenFlow = do
  hideLoaderFlow
  flow <- UI.welcomeScreen
  case flow of
    GoToMobileNumberScreen -> enterMobileNumberScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{config = config}})
  disabilityListT <- updateDisabilityList "Account_Set_Up_Screen"
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{disabilityOptions{disabilityOptionList = disabilityListT }}})
  flow <- UI.accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let gender = getGenderValue state.data.gender
          selectedDisability = state.data.disabilityOptions.selectedDisability
          (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{firstName = (Just state.data.name),gender = gender, hasDisability = Just (isJust selectedDisability), disability = case selectedDisability of 
            Just disability -> Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.disabilityOptions.otherDisabilityReason))
            _ -> Nothing  }
      setValueToLocalStore DISABILITY_UPDATED "true"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{showDisabilityPopUp = (isJust selectedDisability)} , data{disability = selectedDisability}})
      case gender of
          Just value -> void $ pure $ setCleverTapUserData "gender" value
          Nothing -> pure unit

      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          void $ pure $ setCleverTapUserData "Name" (getValueToLocalStore USER_NAME)
          case gender of
            Just value -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just value}}, props{isBanner = false}})
            Nothing    -> pure unit
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_onboarded"
          _ <- pure $ metaLogEvent "ny_user_onboarded"
          pure unit
        Left err -> do
          _ <- pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> state{props{btnActive = true},data{name=state.data.name}})
          accountSetUpScreenFlow
    GO_BACK -> do
      _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      _ <- pure $ deleteValueFromLocalStore MOBILE_NUMBER
      modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> AccountSetUpScreenData.initData)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen -> enterMobileNumberScreen{data{ otp = ""}})
      enterMobileNumberScreenFlow


updateDisabilityList :: String -> FlowBT String (Array DisabilityT)
updateDisabilityList screenType = do
  response <- Remote.disabilityList
  case response of 
    Right (GetDisabilityListResp resp) -> pure $ getDisabilityList resp
    Left err -> pure $ getDisabilityList []

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState currentState) <- getState
  _ <- checkAndUpdateSavedLocations currentState.homeScreen
  _ <- pure $ cleverTapSetLocation unit
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})

  -- TODO: HANDLE LOCATION LIST INITIALLY
  _ <- pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{hasTakenRide = if (getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE") then true else false, isReferred = if (getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE") then true else false }, data {config = config}})
  flow <- UI.homeScreen
  case flow of
    CHECK_FLOW_STATUS -> currentFlowStatus
    GO_TO_MY_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen{data{offsetValue = 0}})
      myRidesScreenFlow true
    GO_TO_HELP -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> HelpAndSupportScreenData.initData)
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_help"
      helpAndSupportScreenFlow
    CHANGE_LANGUAGE ->  selectLanguageScreenFlow
    GO_TO_EMERGENCY_CONTACTS -> do
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> EmergencyContactsScreenData.initData)
      emergencyScreenFlow
    GO_TO_ABOUT -> aboutUsScreenFlow
    GO_TO_MY_TICKETS -> do
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      void $ pure $ spy "bookedRes" bookedRes
      void $ pure $ spy "pendingRes" pendingRes
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData{props{currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes)}})            
      zooTicketBookingFlow
    GO_TO_MY_PROFILE  updateProfile -> do
        _ <- lift $ lift $ liftFlow $ logEvent logField_ (if updateProfile then "safety_banner_clicked" else "ny_user_profile_click")
        modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData{props{fromHomeScreen = updateProfile , updateProfile = updateProfile, changeAccessibility = true, isBtnEnabled = true , genderOptionExpanded = false , showOptions = false, expandEnabled = true }})
        myProfileScreenFlow
    GO_TO_FIND_ESTIMATES updatedState -> do
      if updatedState.data.source == getString CURRENT_LOCATION then do
        PlaceName address <- getPlaceName updatedState.props.sourceLat updatedState.props.sourceLong HomeScreenData.dummyLocation
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{ data{ source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing } })
      else
        pure unit
      (GlobalState globalState) <- getState
      let state = globalState.homeScreen
      liftFlowBT $  logEventWithTwoParams logField_ "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong)
      if (not sourceServiceabilityResp.serviceable) then do
        updateLocalStage SearchLocationModel
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel ,rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false, city = sourceServiceabilityResp.city }})
        homeScreenFlow
        else pure unit
      let currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")
          currentDate =  getCurrentDate ""
      void $ pure $ setCleverTapUserProp [{key : "Latest Search From", value : unsafeToForeign ("lat: " <> (show updatedState.props.sourceLat) <> " long: " <> (show updatedState.props.sourceLong))},
                                          {key : "Latest Search", value : (unsafeToForeign $ currentDate <> " " <> currentTime)}]
      (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress state.props.hotSpot.manuallyMoved state.props.isSpecialZone)
      routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (Remote.normalRoute "") "NORMAL" state.data.source state.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "" false getPolylineAnimationConfig) 
      case rideSearchRes.routeInfo of
        Just (Route response) -> do
          let distance = if response.distance < 1000 then toStringJSON(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
              duration = (show (response.duration / 60)) <> " min"
              Snapped points = response.points
          case head points, last points of
            Just (LatLong source), Just (LatLong dest) -> do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : state.data.source, address : Nothing, city : Nothing }, destination : { lat : dest.lat, lng : dest.lon, place : state.data.destination, address : Nothing, city : Nothing } }) } })
            _ , _ -> pure unit
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration, source = state.data.source, sourceAddress = state.data.sourceAddress}})
          let distanceBtwCurrentAndSource = getDistanceBwCordinates state.props.sourceLat state.props.sourceLong state.props.currentLocation.lat state.props.currentLocation.lng
              isDistMoreThanThreshold = distanceBtwCurrentAndSource > state.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold
          if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000) then do
            updateLocalStage DistanceOutsideLimits
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation, findingQuotesProgress = 0.0, isShorterTrip = false}})
            homeScreenFlow
            else if ( (response.distance < 500  || isDistMoreThanThreshold )&& Arr.all (_ == false ) [ isLocalStageOn PickUpFarFromCurrentLocation , isLocalStageOn ShortDistance]) then do 
              let currentStage = if isDistMoreThanThreshold then PickUpFarFromCurrentLocation else ShortDistance
              updateLocalStage currentStage
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = currentStage ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance, isShorterTrip = response.distance < 500, findingQuotesProgress = 0.0}})
              homeScreenFlow
            else pure unit
          pure unit
        Nothing -> pure unit
      void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : state.props.sourceLat, lng : state.props.sourceLong, place : state.data.source, address : Nothing, city : state.props.city}
                                                      , destination : {lat : state.props.destinationLat, lng : state.props.destinationLong, place : state.data.destination, address : Nothing, city : Nothing}
                                                      , sourceAddress : state.data.sourceAddress
                                                      , destinationAddress : state.data.destinationAddress })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, findingQuotesProgress = 0.0}, data{nearByDrivers = Nothing}})
      updateLocalStage FindingEstimate
      homeScreenFlow
    RETRY_FINDING_QUOTES showLoader-> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
      void $ lift $ lift $ toggleLoader showLoader
      (GlobalState newState) <- getState
      let state = newState.homeScreen
      liftFlowBT $ logEventWithParams logField_ "ny_user_tip_search" "Tip amount (₹)" (show $ state.props.customerTip.tipForDriver)
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_retry_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                      {key : "Estimate Fare (₹)", value : unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare)},
                                                                                                      {key : "Customer tip (₹)", value : unsafeToForeign state.props.customerTip.tipForDriver},
                                                                                                      {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                      {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges}]
      if (not (isLocalStageOn QuoteList)) then do
        void $ pure $ firebaseLogEvent "ny_user_cancel_and_retry_request_quotes"
        cancelEstimate state.props.estimateId
      else do
        void $ pure $ firebaseLogEvent "ny_user_retry_request_quotes"
      setValueToLocalStore AUTO_SELECTING "false"
      setValueToLocalStore FINDING_QUOTES_POLLING "false"
      setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
      when (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") do
        void $ pure $ firebaseLogEvent "ny_user_auto_confirm"

      let currentTime = (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")
          findingQuotesTime = convertUTCtoISC (getValueToLocalNativeStore FINDING_QUOTES_START_TIME) "HH:mm:ss"
      if withinTimeRange findingQuotesTime currentTime "22:00:00" || withinTimeRange findingQuotesTime currentTime "05:00:00" then do
        void $ pure $ toast (getString PLEASE_FIND_REVISED_FARE_ESTIMATE)
        void $ pure $ firebaseLogEvent "ny_user_new_estimate_after_night_charges_applicable"
        updateLocalStage FindEstimateAndSearch
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = false } })
      else do
        void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
        response <- lift $ lift $ Remote.selectEstimate (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
        case response of
          Right res -> do
            updateLocalStage FindingQuotes
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } })
          Left err -> do
            void $ pure $ firebaseLogEvent "ny_user_estimate_expired"
            updateLocalStage FindEstimateAndSearch
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = true } })
        let tipViewData = if state.props.customerTip.isTipSelected then state.props.tipViewProps else HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { customerTip = if homeScreen.props.customerTip.isTipSelected then homeScreen.props.customerTip else HomeScreenData.initData.props.customerTip{enableTips = homeScreen.props.customerTip.enableTips } , tipViewProps = tipViewData, findingQuotesProgress = 0.0 }})
      homeScreenFlow
    LOCATION_SELECTED item addToRecents-> do
        void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
        void $ lift $ lift $ toggleLoader true
        (GlobalState newState) <- getState
        let state = newState.homeScreen

        case state.props.sourceSelectedOnMap of
          true | state.props.isSource == Just true -> pure unit
          _ -> 
            case state.props.isSource of
              Just true -> do
                (GetPlaceNameResp sourceDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.sourcePlaceId state.props.sourceLat state.props.sourceLong (if state.props.isSource == Just false then dummyLocationListItemState else item)
                let (PlaceName sourceDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (sourceDetailResp !! 0))
                    (LatLong sourceLocation) = sourceDetailResponse.location
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{sourceLat = sourceLocation.lat, sourceLong = sourceLocation.lon} })
              Just false  -> do
                (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.destinationPlaceId state.props.destinationLat state.props.destinationLong (if state.props.isSource == Just true then dummyLocationListItemState else item)
                let (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp!!0))
                    (LatLong destinationLocation) = (destinationDetailResponse.location)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon} })
              _          -> pure unit
        updateSourceLocation ""
        (GlobalState updatedState) <- getState
        let bothLocationChangedState = updatedState.homeScreen
        (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong)
        let srcServiceable = sourceServiceabilityResp.serviceable
        let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
        let pickUpPoints = map (\(GatesInfo item) -> {
                                                place: item.name,
                                                lat  : (item.point)^._lat,
                                                lng : (item.point)^._lon,
                                                address : item.address,
                                                city : Nothing
                                              }) srcSpecialLocation.gates
        (ServiceabilityResDestination destServiceabilityResp) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong)
        let destServiceable = destServiceabilityResp.serviceable
        let pickUpLoc = if length pickUpPoints > 0 then (if state.props.defaultPickUpPoint == "" then fetchDefaultPickupPoint pickUpPoints state.props.sourceLat state.props.sourceLong else state.props.defaultPickUpPoint) else (fromMaybe HomeScreenData.dummyLocation (state.data.nearByPickUpPoints!!0)).place
        modifyScreenState $ HomeScreenStateType (\homeScreen -> bothLocationChangedState{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson,nearByPickUpPoints=pickUpPoints},props{city = sourceServiceabilityResp.city , isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing, confirmLocationCategory = if length pickUpPoints > 0 then state.props.confirmLocationCategory else "", findingQuotesProgress = 0.0 }})
        if (addToRecents) then
          addLocationToRecents item bothLocationChangedState sourceServiceabilityResp.serviceable destServiceabilityResp.serviceable
          else pure unit
        (GlobalState globalState) <- getState
        let updateScreenState = globalState.homeScreen
        if (not srcServiceable && (updateScreenState.props.sourceLat /= -0.1 && updateScreenState.props.sourceLong /= -0.1) && (updateScreenState.props.sourceLat /= 0.0 && updateScreenState.props.sourceLong /= 0.0)) then do
          let recentList = recentDistance updateScreenState.data.recentSearchs.predictionArray updateScreenState.props.sourceLat updateScreenState.props.sourceLong
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isSrcServiceable = false, isRideServiceable= false, isSource = Just true}, data {recentSearchs {predictionArray = recentList}}})
          homeScreenFlow
        else if ((not destServiceable) && (updateScreenState.props.destinationLat /= 0.0 && updateScreenState.props.destinationLat /= -0.1) && (updateScreenState.props.destinationLong /= 0.0 && bothLocationChangedState.props.destinationLong /= -0.1)) then do
          if (getValueToLocalStore LOCAL_STAGE == "HomeScreen") then do
            _ <- pure $ toast (getString LOCATION_UNSERVICEABLE)
            pure unit
            else pure unit
          let recentList = (recentDistance updateScreenState.data.recentSearchs.predictionArray updateScreenState.props.sourceLat updateScreenState.props.sourceLong)
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isDestServiceable = false, isRideServiceable = false,isSource = Just false, isSrcServiceable = true}, data {recentSearchs {predictionArray = recentList}}})
          homeScreenFlow
         else modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{ isRideServiceable= true, isSrcServiceable = true, isDestServiceable = true}, data {recentSearchs {predictionArray = (recentDistance updateScreenState.data.recentSearchs.predictionArray updateScreenState.props.sourceLat updateScreenState.props.sourceLong)}}})
        rideSearchFlow "NORMAL_FLOW"

    SEARCH_LOCATION input state -> do
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( state.props.sourceLat) ( state.props.sourceLong) getSearchRadius (EHC.getMapsLanguageFormat $ getValueToLocalStore LANGUAGE_KEY) "")
      let event =
            case state.props.isSource of
              Just true -> "ny_user_auto_complete_api_trigger_src"
              Just false -> "ny_user_auto_complete_api_trigger_dst"
              Nothing -> ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ event
      let sortedByDistanceList = sortPredctionByDistance searchLocationResp.predictions
      let predictionList = getLocationList sortedByDistanceList
      let recentLists =  recentDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong
      let filteredRecentsList = filterRecentSearches recentLists predictionList
      let filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList =
        map
        (\item -> do
                  let savedLocation  = getPrediction item state.data.savedLocations
                      locIsPresentInSavedLoc = checkPrediction item state.data.savedLocations
                  if not locIsPresentInSavedLoc then
                    item {
                      lat = savedLocation.lat,
                      lon = savedLocation.lon,
                      locationItemType = Just SAVED_LOCATION,
                      postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav_red" }
                    else
                      item {
                        lat = item.lat,
                        lon = item.lon,
                        locationItemType = item.locationItemType,
                        postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav" }
            ) ((filteredRecentsList) <> filteredPredictionList) }, props{searchLocationModelProps{isAutoComplete = true,  showLoader = false}}})
      homeScreenFlow
    GET_QUOTES state -> do
          setValueToLocalStore AUTO_SELECTING "false"
          setValueToLocalStore LOCAL_STAGE (show FindingQuotes)
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          liftFlowBT $ logEvent logField_ "ny_user_request_quotes"
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                          {key : "Estimate Fare (₹)", value : unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare)},
                                                                                                          {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                          {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges}]
          if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
            _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_auto_confirm"
            pure unit
          else do
            pure unit
          void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
          _ <- Remote.selectEstimateBT (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
          homeScreenFlow
    SELECT_ESTIMATE state -> do
        updateLocalStage SettingPrice
        let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
            destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
            srcMarker = (Remote.normalRoute "").srcMarker
            destMarker = (Remote.normalRoute "").destMarker
        case state.props.routeEndPoints of
          Just points -> lift $ lift $ liftFlow $ updateRouteMarker $ updateRouteMarkerConfig (Remote.walkCoordinate points.source.lat points.source.lng points.destination.lat points.destination.lng) points.source.place points.destination.place srcMarker destMarker (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig)
          Nothing -> pure unit
        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList) $ do
        updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
          _ <- pure $ enableMyLocation false
          let selectedQuote = if state.props.isSpecialZone && state.data.currentSearchResultType == QUOTES then state.data.specialZoneSelectedQuote else state.props.selectedQuote
          if isJust selectedQuote then do
            updateLocalStage ConfirmingRide
            response  <- lift $ lift $ Remote.rideConfirm (fromMaybe "" selectedQuote)
            case response of
              Right (ConfirmRes resp) -> do
                let bookingId = resp.bookingId
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp}})
                homeScreenFlow
              Left err  -> do
                if not (err.code == 400 && (decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED") then pure $ toast (getString ERROR_OCCURED_TRY_AGAIN) else pure unit
                _ <- setValueToLocalStore AUTO_SELECTING "false"
                _ <- pure $ updateLocalStage QuoteList
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = QuoteList,selectedQuote = Nothing, expiredQuotes = snoc state.props.expiredQuotes (fromMaybe "" state.props.selectedQuote)}, data {quoteListModelState = []}})
                homeScreenFlow
            else homeScreenFlow
    ONGOING_RIDE state -> do
      setValueToLocalStore TRACKING_ENABLED "True"
      setValueToLocalStore TRACKING_DRIVER "False"
      setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
      let srcLat = state.data.driverInfoCardState.sourceLat
          srcLon = state.data.driverInfoCardState.sourceLng
          dstLat = state.data.driverInfoCardState.destinationLat
          dstLon = state.data.driverInfoCardState.destinationLng
      updateLocalStage state.props.currentStage
      if spy "ONGOING_RIDEONGOING_RIDE CURRENT" state.props.currentStage == RideCompleted then
        do
          let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
              destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
          _ <- pure $ spy "INSIDE IF OF ONGOING" state.props.currentStage
          _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "DRIVER_LOCATION_UPDATE" "" "" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon true getPolylineAnimationConfig) 
          homeScreenFlow
        else if state.props.currentStage == HomeScreen then
          do
            _ <- pure $ removeAllPolylines ""
            _ <- pure $ spy "INSIDE ELSE IF OF ONGOING" state.props.currentStage
            _ <- updateLocalStage HomeScreen
            updateUserInfoToState state
            homeScreenFlow
          else do
            lift $ lift $ triggerRideStatusEvent "DRIVER_ASSIGNMENT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
            homeScreenFlow
    CANCEL_RIDE_REQUEST state -> do
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_rider_cancellation" $ [ {key : "Reason code", value : unsafeToForeign state.props.cancelReasonCode},
                                                                                                      {key : "Additional info", value : unsafeToForeign state.props.cancelDescription},
                                                                                                      {key : "Pickup", value : unsafeToForeign state.data.driverInfoCardState.source},
                                                                                                      {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                      {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges}]
      _ <- Remote.cancelRideBT (Remote.makeCancelRequest state) (state.props.bookingId)
      lift $ lift $ triggerRideStatusEvent "CANCELLED_PRODUCT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
      liftFlowBT $ logEvent logField_ "ny_user_ride_cancelled_by_user"
      liftFlowBT $ logEvent logField_ $ "ny_user_cancellation_reason: " <> state.props.cancelReasonCode
      removeChatService ""
      updateUserInfoToState state
      homeScreenFlow
    FCM_NOTIFICATION notification state-> do
        let rideID = state.data.driverInfoCardState.rideId
            srcLat = state.data.driverInfoCardState.sourceLat
            srcLon = state.data.driverInfoCardState.sourceLng
            dstLat = state.data.driverInfoCardState.destinationLat
            dstLon = state.data.driverInfoCardState.destinationLng
        setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
        setValueToLocalStore FINDING_QUOTES_POLLING "false"
        setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          setValueToLocalStore TRACKING_ENABLED "False"
          pure unit
          else do
            setValueToLocalStore TRACKING_ENABLED "True"
            pure unit
        case notification of
            "TRIP_STARTED"        -> do -- OTP ENTERED
                                      checkRideStatus true
                                      (GlobalState updatedState) <- getState
                                      let homeScreenState = updatedState.homeScreen
                                      when (homeScreenState.props.currentStage == RideStarted) $ do 
                                        let shareAppCount = getValueToLocalStore SHARE_APP_COUNT
                                        if shareAppCount == "__failed" then do
                                          setValueToLocalStore SHARE_APP_COUNT "1"
                                        else if shareAppCount /= "-1" then do
                                          setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount))))+1))
                                        else pure unit
                                        _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                        let newState = homeScreenState{data{route = Nothing},props{isCancelRide = false,waitingTimeTimerIds = [], showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0, showChatNotification = false, cancelSearchCallDriver = false  }}
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> newState)
                                        lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      homeScreenFlow
            "TRIP_FINISHED"       -> do -- TRIP FINISHED
                                      if (getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "false") then do
                                        _ <- pure $ metaLogEvent "ny_user_first_ride_completed"
                                        (GetProfileRes response) <- Remote.getProfileBT ""
                                        setValueToLocalStore HAS_TAKEN_FIRST_RIDE ( show response.hasTakenRide)
                                        else pure unit
                                      let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
                                          destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
                                      _ <- pure $ metaLogEvent "ny_user_ride_completed"
                                      _ <- updateLocalStage HomeScreen
                                      if (state.props.bookingId /= "") then do
                                        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
                                        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                                        let (RideBookingDetails contents) = bookingDetails.contents
                                        let (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                                        let finalAmount =  getFinalAmount (RideBookingRes resp)
                                        let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
                                        lift $ lift $ triggerRideStatusEvent notification (Just finalAmount) (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                        setValueToLocalStore PICKUP_DISTANCE "0"
                                        liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_ride_completed" (rideCompletedDetails (RideBookingRes resp))
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{startedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime ) "h:mm A", startedAtUTC = fromMaybe "" resp.rideStartTime ,endedAt = convertUTCtoISC (fromMaybe "" resp.rideEndTime ) "h:mm A", finalAmount = finalAmount, rideRatingState {driverName = ride.driverName, rideId = ride.id , distanceDifference = differenceOfDistance} , ratingViewState { rideBookingRes = (RideBookingRes resp)}, driverInfoCardState {initDistance = Nothing}},props{currentStage = RideCompleted, estimatedDistance = contents.estimatedDistance}})
                                        homeScreenFlow
                                        else homeScreenFlow
            "CANCELLED_PRODUCT"   -> do -- REMOVE POLYLINES
                                      _ <- pure $ removeAllPolylines ""
                                      _ <- updateLocalStage HomeScreen
                                      removeChatService ""
                                      setValueToLocalStore PICKUP_DISTANCE "0"
                                      lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      updateUserInfoToState state
                                      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                      permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
                                      permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
                                      if not (permissionConditionA && permissionConditionB) then do
                                        modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                                        permissionScreenFlow
                                      else homeScreenFlow
            "DRIVER_ASSIGNMENT"   -> if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted )) then do
                                        setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
                                        _ <- liftFlowBT $ logEvent logField_ "ny_fs_driver_assignment"
                                        lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                        checkRideStatus true
                                        homeScreenFlow
                                     else homeScreenFlow
            _                     -> homeScreenFlow

    LOGOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      _ <- pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
      _ <- pure $ deleteValueFromLocalStore CUSTOMER_ID
      _ <- pure $ deleteValueFromLocalStore CONTACTS
      _ <- pure $ deleteValueFromLocalStore USER_EMAIL
      _ <- pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_logout"
      _ <- pure $ (setText (getNewIDWithTag "EnterMobileNumberEditText") "" )
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> EnterMobileNumberScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      enterMobileNumberScreenFlow -- Removed choose langauge screen
    SUBMIT_RATING state -> do
      liftFlowBT $ logEventWithParams logField_ "ny_user_ride_give_feedback" "Rating" (show $ state.data.rating)
      _ <- Remote.bookingFeedbackBT (Remote.makeRideFeedBackReq (state.data.rideRatingState.rideId) (state.data.rideRatingState.feedbackList))
      _ <- Remote.rideFeedbackBT (Remote.makeFeedBackReq (state.data.rideRatingState.rating) (state.data.rideRatingState.rideId) (state.data.rideRatingState.feedback))
      _ <- updateLocalStage HomeScreen
      let finalAmount = if state.data.finalAmount == 0 then state.data.rideRatingState.finalAmount else state.data.finalAmount
      let bookingId = if state.props.bookingId == "" then state.data.rideRatingState.bookingId else state.props.bookingId
      pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
                                          event : "process_result"
                                        , payload : Just {
                                          action : "feedback_submitted"
                                        , trip_amount : Just finalAmount
                                        , trip_id : Just bookingId
                                        , ride_status : Nothing
                                        , screen : Just $ getScreenFromStage state.props.currentStage
                                        , exit_app : false
                                        }
                                        }
      updateUserInfoToState state
      if state.props.currentStage == RideCompleted then
        if (getSearchType unit) == "direct_search" then do
          _ <- updateLocalStage SearchLocationModel
          checkAndUpdateLocations
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel}})
          else pure unit
        else pure unit
      if state.data.rideRatingState.rating == 5 then do
        _ <- pure $ launchInAppRatingPopup unit
        pure unit
        else pure unit
      homeScreenFlow
    CANCEL -> homeScreenFlow
    RELOAD saveToCurrLocs -> do
      (GlobalState state) <- getState
      if state.homeScreen.props.currentStage == SearchLocationModel then do
        if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
        _ <- pure $ toggleBtnLoader "" false
        homeScreenFlow
        else do
          if state.homeScreen.props.sourceLat/=0.0 && state.homeScreen.props.sourceLong /= 0.0 then do
            (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong)
            -- let srcServiceable = sourceServiceabilityResp.serviceable
            let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
            let pickUpPoints = map (\(GatesInfo item) -> {
                                                    place: item.name,
                                                    lat  : (item.point)^._lat,
                                                    lng : (item.point)^._lon,
                                                    address : item.address,
                                                    city : Nothing
                                                  }) srcSpecialLocation.gates
            if (sourceServiceabilityResp.serviceable ) then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{ polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson, nearByPickUpPoints = pickUpPoints } ,props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then
                addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source
              else pure unit
            else do
              _ <- pure $ firebaseLogEvent "ny_loc_unserviceable"
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = false, showlocUnserviceablePopUp = true}})
          else do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})

      homeScreenFlow
    RETRY  -> homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long-> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat long)
      let sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat
          sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong
      _ <- pure $ firebaseLogEvent $ "ny_loc_unserviceable_" <> show (not sourceServiceabilityResp.serviceable)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{data{ polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson } ,props{locateOnMapLocation{sourceLat = sourceLat, sourceLng = sourceLong, source= (getString CURRENT_LOCATION)}, sourceLat = sourceLat, sourceLong = sourceLong, isSrcServiceable =sourceServiceabilityResp.serviceable , showlocUnserviceablePopUp = (not sourceServiceabilityResp.serviceable)}})
      homeScreenFlow
    HOME_SCREEN -> do
        (GlobalState state) <- getState
        when (isLocalStageOn FindingQuotes) $ do
          cancelEstimate state.homeScreen.props.estimateId
        _ <- pure $ removeAllPolylines ""
        _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
        _ <- pure $ currentPosition ""
        _ <- updateLocalStage HomeScreen
        updateUserInfoToState state.homeScreen
        homeScreenFlow
    CHECK_CURRENT_STATUS -> do
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes) $ do
        cancelEstimate state.homeScreen.props.estimateId
      _ <- pure $ removeAllPolylines ""
      _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      updateUserInfoToState state.homeScreen
      currentFlowStatus
    UPDATE_LOCATION_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address,
                                              city : Nothing
                                            }) srcSpecialLocation.gates
      checkForSpecialZoneAndHotSpots state (ServiceabilityRes sourceServiceabilityResp) lat lon
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      let cachedLat = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLat else state.props.locateOnMapLocation.destinationLat)
          cachedLon = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLng else state.props.locateOnMapLocation.destinationLng)
          cachedLocation = (if state.props.isSource == Just true then state.props.locateOnMapLocation.source else state.props.locateOnMapLocation.destination)
          distanceBetweenLatLong = getDistanceBwCordinates lat lon cachedLat cachedLon
          isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius/1000.0) 
      modifyScreenState $ HomeScreenStateType (\homeScreen ->
          homeScreen { 
            props {
              city = if state.props.isSource == Just true then sourceServiceabilityResp.city else Nothing, 
              sourcePlaceId = if state.props.isSource == Just true then Nothing else homeScreen.props.sourcePlaceId,
              destinationPlaceId = if state.props.isSource == Just false then Nothing else homeScreen.props.destinationPlaceId,
              destinationLat = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lat else state.props.destinationLat,
              destinationLong = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lon else state.props.destinationLong,
              sourceLat = if state.props.isSource == Just true then lat else state.props.sourceLat,
              sourceLong = if state.props.isSource == Just true then lon else state.props.sourceLong,
              confirmLocationCategory = srcSpecialLocation.category
              }
            })
      if isMoreThan20Meters || cachedLocation == "" then do
        PlaceName placeDetails <- getPlaceName lat lon gateAddress
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_api_lom_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then placeDetails.formattedAddress else homeScreen.data.destination,
            source = if state.props.isSource == Just true then placeDetails.formattedAddress else homeScreen.data.source,
            sourceAddress = case state.props.isSource , (state.props.currentStage /= ConfirmingLocation) of
              Just true, true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing
              _ , _-> encodeAddress homeScreen.data.source [] state.props.sourcePlaceId,
            destinationAddress = case state.props.isSource,(state.props.currentStage /= ConfirmingLocation) of
              Just false , true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing
              _ , _ -> encodeAddress homeScreen.data.destination [] state.props.destinationPlaceId
          }
          })
      else do
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_cache_lom_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
          homeScreen {
            data {
              destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then state.props.locateOnMapLocation.destination else homeScreen.data.destination,
              source = if state.props.isSource == Just true then state.props.locateOnMapLocation.source else homeScreen.data.source,
              sourceAddress = case state.props.isSource , (state.props.currentStage /= ConfirmingLocation) of
                Just true, true -> state.props.locateOnMapLocation.sourceAddress 
                _ , _-> state.data.sourceAddress, 
              destinationAddress = case state.props.isSource,(state.props.currentStage /= ConfirmingLocation) of
                Just false , true -> state.props.locateOnMapLocation.destinationAddress 
                _ , _ -> state.data.destinationAddress 
            }
            })
      let _ = spy "UPDATE_LOCATION_NAME" "UPDATE_LOCATION_NAME"
      homeScreenFlow
    UPDATE_PICKUP_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address,
                                              city : Nothing
                                            }) srcSpecialLocation.gates
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      let geoJson = fromMaybe "" sourceServiceabilityResp.geoJson

      if not (DS.null geoJson) && not (null pickUpPoints) then do
        _ <- pure $ spy "debug hotspot cond 1" "."
        when (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) $ do
          _ <- pure $ spy "debug hotspot cond 2" "."
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = geoJson, nearByPickUpPoints = pickUpPoints},props{city = sourceServiceabilityResp.city, isSpecialZone =  not (DS.null geoJson) , confirmLocationCategory = srcSpecialLocation.category}})
          _ <- pure $ removeAllPolylines ""
          liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = (fromMaybe "" sourceServiceabilityResp.geoJson), points = pickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
          homeScreenFlow
      else if not (null sourceServiceabilityResp.hotSpotInfo) then do
        _ <- pure $ spy "debug hotspot cond 4" "."
        let hotSpotInfo = spy "debug hotspot data" $ transformHotSpotInfo (filter (\(HotSpotInfo hotSpot) ->
                                                                                      let (LatLong point) = hotSpot.centroidLatLong
                                                                                          distance = (getDistanceBwCordinates lat lon point.lat point.lon) * 1000.0
                                                                                      in
                                                                                        distance < 100.0
                                                                                  ) sourceServiceabilityResp.hotSpotInfo)
        let points = map (\item -> {  place: "",
                                        lat  : item.lat,
                                        lng : item.lon,
                                        address : Nothing,
                                        city : Nothing
                                      }) hotSpotInfo
        when (state.data.nearByPickUpPoints /= points && not (null points)) $ do
          _ <- pure $ spy "debug hotspot cond 5" "."
          pure $ removeAllPolylines ""
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = "", nearByPickUpPoints = points},props{isSpecialZone = false, defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (points!!0)).place, confirmLocationCategory = srcSpecialLocation.category}})
          liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = "", points = points, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
          -- homeScreenFlow
      else do
        _ <- pure $ spy "debug hotspot cond 7" "."
        pure unit
      let distanceBetweenLatLong = getDistanceBwCordinates lat lon state.props.locateOnMapLocation.sourceLat state.props.locateOnMapLocation.sourceLng
          isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius/1000.0)
      modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          props {
            sourceLat = lat ,
            sourceLong = lon,
            confirmLocationCategory = srcSpecialLocation.category,
            city = sourceServiceabilityResp.city
            }
          })
      if isMoreThan20Meters then do
        _ <- pure $ spy "debug hotspot cond 9" "."
        PlaceName address <- getPlaceName lat lon gateAddress
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_api_cpu_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            source = address.formattedAddress ,
            sourceAddress = encodeAddress address.formattedAddress address.addressComponents Nothing }
          })
      else do
        _ <- pure $ spy "debug hotspot cond 10" "."
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_cache_cpu_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            source = state.props.locateOnMapLocation.source,
            sourceAddress = state.props.locateOnMapLocation.sourceAddress
          }
        })
      homeScreenFlow
    GO_TO_FAVOURITES_  -> do
        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_addresses"
        savedLocationFlow
    OPEN_GOOGLE_MAPS state -> do
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_gmaps"
      (GetDriverLocationResp resp) <- Remote.getDriverLocationBT (state.data.driverInfoCardState.rideId)
      let sourceLat = (resp^._lat)
          sourceLng = (resp^._lon)
          destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
          destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
      _ <- lift $ lift $ fork $ liftFlow $ openNavigation sourceLat sourceLng destLat destLng "DRIVE"
      homeScreenFlow
    IN_APP_TRACK_STATUS state -> do
      case state.props.currentStage of
          RideAccepted -> do
                          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_pickup_track_inapp"
                          pure unit
          RideStarted  -> do
                          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_inapp"
                          pure unit
          _           -> pure unit
      if (spy "driver current Stage "isLocalStageOn RideAccepted) || (spy "driver current Stage " isLocalStageOn RideStarted) then do
        setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          setValueToLocalStore TRACKING_ENABLED "False"
          homeScreenFlow
          else do
            setValueToLocalStore TRACKING_ENABLED "True"
            homeScreenFlow
        else
          homeScreenFlow
    UPDATE_SAVED_LOCATION -> do
      savedLocationResp <- lift $ lift $ Remote.getSavedLocationList ""
      updateSourceLocation ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_home_blue" || (listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_work_blue"))) (AddNewAddress.getSavedLocations listResp.list))
              recents = (differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag)
              savedLocationsWithOtherTag = (filter (\listItem -> not(listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_home_blue" || listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_work_blue")) (AddNewAddress.getSavedLocations listResp.list))
              updatedList = (map (\item ->  item { postfixImageUrl = if not (checkPrediction item savedLocationsWithOtherTag) then fetchImage FF_ASSET "ny_ic_fav_red"
                                                                        else fetchImage FF_ASSET "ny_ic_fav" }) (recents))
              predicArray = (recentDistance updatedList currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong)
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen
                                                                    { data
                                                                      { savedLocations = (AddNewAddress.getSavedLocations listResp.list)
                                                                      , recentSearchs {predictionArray = predicArray}
                                                                      , locationList = predicArray
                                                                      }
                                                                    })
          homeScreenFlow
        Left (err) -> homeScreenFlow

    GO_TO_INVOICE_ updatedState -> do
      let prevRideState = updatedState.data.rideRatingState
      let finalAmount = show prevRideState.finalAmount
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen= true},data{totalAmount = ((getValueFromConfig "currency") <> " " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ((getValueFromConfig "currency") <> " " <> finalAmount), selectedItem {date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId,rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId,vehicleNumber = prevRideState.vehicleNumber,time = prevRideState.rideStartTime,source = prevRideState.source,destination = prevRideState.destination,driverName = prevRideState.driverName,totalAmount = ((getValueFromConfig "currency") <> " " <> finalAmount)}, config = updatedState.data.config}})
      invoiceScreenFlow

    CHECK_FOR_DUPLICATE_SAVED_LOCATION state -> do
      let recents = map
                    (\item -> item{postfixImageVisibility = false, postfixImageUrl = ""}
                      ) (differenceOfLocationLists (state.data.recentSearchs.predictionArray) state.data.savedLocations)

      case state.data.selectedLocationListItem of
        Nothing -> do
          modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { props
                { showSavePlaceView = false
                , fromHome = true
                , editLocation = false
                , editSavedLocation = false
                , isLocateOnMap = false
                , isBtnActive = true
                , isSearchedLocationServiceable = true
                , tagExists = false
                , placeNameExists = false }
              , data
                { addressSavedAs = ""
                , placeName = ""
                , savedLocations = state.data.savedLocations
                , locationList = recents
                , recentSearchs{predictionArray = recents}
                , selectedTag = state.props.tagType
                , savedTags = AddNewAddress.getSavedTagsFromHome state.data.savedLocations
                , address = ""
                , activeIndex = case state.props.tagType of
                                  Just tag -> case tag of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                _        -> Just 2
                                  Nothing  -> Nothing }})
          addNewAddressScreenFlow ""
        Just selectedLocationListItem -> do
          case selectedLocationListItem.locationItemType of
            Just RECENTS ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            Nothing ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            _ -> do
              (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedLocationListItem.title <> ", " <> selectedLocationListItem.subTitle) selectedLocationListItem.placeId (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon) selectedLocationListItem

              let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
              let (LatLong placeLatLong) = (placeName.location)

              (ServiceabilityResDestination serviceabilityRes) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest (placeLatLong.lat) (placeLatLong.lon))
              case (serviceabilityRes.serviceable) of
                false -> do
                          _ <- pure $ toast ("Location Unserviceable")
                          homeScreenFlow
                _     ->   modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{ selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}})
              getDistanceDiff  state{data{ saveFavouriteCard{selectedItem{lat = Just (placeLatLong.lat), lon =Just (placeLatLong.lon) }},selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}} (placeLatLong.lat) (placeLatLong.lon)
    GO_TO_CALL_EMERGENCY_CONTACT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "EmergencyContact" state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo) state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_POLICE state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "Police" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_SUPPORT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "CustomerCare" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_SOS_STATUS state -> do
        res <- Remote.userSosStatusBT state.props.emergencyHelpModelState.sosId (Remote.makeSosStatus state.props.emergencyHelpModelState.sosStatus)
        homeScreenFlow
    GO_TO_FETCH_CONTACTS state-> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) res.defaultEmergencyNumbers
      contactsInString <- pure $ toStringJSON contacts
      setValueToLocalStore CONTACTS (contactsInString)
      contactsInJson <- pure $ parseNewContacts contactsInString
      let newContacts = transformContactList contactsInJson
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{emergencyContactData = newContacts}}})
      homeScreenFlow
    SAVE_FAVOURITE state -> do
      let tag = case  (toLower state.data.saveFavouriteCard.tag) of
                  "work" -> "Work"
                  "home" -> "Home"
                  _      -> state.data.saveFavouriteCard.tag
      _ <- setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      case state.data.saveFavouriteCard.selectedItem.lat , state.data.saveFavouriteCard.selectedItem.lon of
        Nothing , Nothing -> fetchLatAndLong state tag
        _ , _ -> do
          resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId state.data.saveFavouriteCard.selectedItem.lat state.data.saveFavouriteCard.selectedItem.lon [])
          pure unit
      _ <-  pure $ toast (getString FAVOURITE_ADDED_SUCCESSFULLY)
      (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          let updatedLocationList = getUpdatedLocationList state.data.locationList state.data.saveFavouriteCard.selectedItem.placeId
          let updatedRecents = getUpdatedLocationList state.data.recentSearchs.predictionArray  state.data.saveFavouriteCard.selectedItem.placeId
          modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList = updatedLocationList, recentSearchs{predictionArray = updatedRecents},savedLocations = (AddNewAddress.getSavedLocations listResp.list)}})
          homeScreenFlow
        Left (err) -> homeScreenFlow
    GO_TO_REFERRAL -> referralScreenFlow
    ON_CALL state callType -> do
      (OnCallRes res) <- Remote.onCallBT (Remote.makeOnCallReq state.data.driverInfoCardState.rideId (show callType))
      homeScreenFlow
    TRIGGER_PERMISSION_FLOW flowType -> do 
      modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen{stage = flowType})
      permissionScreenFlow
    REPORT_ISSUE state -> do
       if isNothing state.data.ratingViewState.issueReason then do
        _ <- Remote.callbackRequestBT FunctionCall
        _ <- pure $ toast $ getString WE_WILL_GIVE_YOU_CALLBACK
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false} }})
        homeScreenFlow
       else do
        _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.props.bookingId) (fromMaybe "" state.data.ratingViewState.issueReason) state.data.ratingViewState.issueDescription )
        _ <- pure $ toast $ getString YOUR_ISSUE_HAS_BEEN_REPORTED
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false, openReportIssue = false} }})
        homeScreenFlow
    RIDE_DETAILS_SCREEN state -> do
      tripDetailsScreenFlow Home
    GO_TO_TICKET_BOOKING_FLOW state -> do 
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{props{currentStage = DescriptionStage, previousStage = DescriptionStage}})
      zooTicketBookingFlow
    _ -> homeScreenFlow

getDistanceDiff :: HomeScreenState -> Number -> Number -> FlowBT String Unit
getDistanceDiff state lat lon = do
  distanceInfo <- getDistanceInfo (state.data.savedLocations) "" (lat) (lon) (fromMaybe "" state.data.saveFavouriteCard.selectedItem.placeId)
  case distanceInfo.locExistsAs of
    "" ->  modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{isSaveFavourite = true}})
    _  -> do
            _ <- pure $ toast  (getString ALREADY_EXISTS)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{saveFavouriteCard{selectedItem = dummyLocationListState}}})
  homeScreenFlow


fetchLatAndLong :: HomeScreenState -> String -> FlowBT String Unit
fetchLatAndLong state tag  =
  case state.data.saveFavouriteCard.selectedItem.placeId of
    Just placeID -> do
      (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (state.data.saveFavouriteCard.selectedItem.title <> ", " <> state.data.saveFavouriteCard.selectedItem.subTitle) (Just placeID) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lat) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lon) state.data.saveFavouriteCard.selectedItem
      let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
      let (LatLong placeLatLong) = (placeName.location)
      resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents)
      pure unit
    Nothing -> pure unit

rideSearchFlow :: String -> FlowBT String Unit
rideSearchFlow flowType = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState homeScreenModifiedState) <- getState
  let finalState = homeScreenModifiedState.homeScreen -- bothLocationChangedState{props{isSrcServiceable =homeScreenModifiedState.homeScreen.props.isSrcServiceable, isDestServiceable = homeScreenModifiedState.homeScreen.props.isDestServiceable, isRideServiceable = homeScreenModifiedState.homeScreen.props.isRideServiceable }}
  if (finalState.props.sourceLat /= 0.0 && finalState.props.sourceLong /= 0.0) && (finalState.props.destinationLat /= 0.0 && finalState.props.destinationLong /= 0.0) && (finalState.data.source /= "") && (finalState.data.destination /= "")
    then do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{flowWithoutOffers = flowWithoutOffers WithoutOffers}})
      case finalState.props.sourceSelectedOnMap of
        false -> do
          pure $ removeAllPolylines ""
          liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = finalState.props.sourceLat, lon = finalState.props.sourceLong, geoJson = finalState.data.polygonCoordinates, points = finalState.data.nearByPickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingLocation,rideRequestFlow = true, locateOnMapLocation{sourceLat = finalState.props.sourceLat, sourceLng = finalState.props.sourceLong, source = finalState.data.source, sourceAddress = finalState.data.sourceAddress}}})
          _ <- pure $ updateLocalStage ConfirmingLocation
          void $ lift $ lift $ toggleLoader false
        true -> do
          let currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")
              currentDate =  getCurrentDate ""
          void $ pure $ setCleverTapUserProp [{key : "Latest Search From", value : unsafeToForeign ("lat: " <> (show finalState.props.sourceLat) <> " long: " <> (show finalState.props.sourceLong))},
                                              {key : "Latest Search", value : unsafeToForeign (currentDate <> " " <> currentTime)}]
          (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong finalState.data.sourceAddress finalState.data.destinationAddress finalState.props.hotSpot.manuallyMoved finalState.props.isSpecialZone)
          void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : finalState.props.sourceLat, lng : finalState.props.sourceLong, place : finalState.data.source, address : Nothing, city : finalState.props.city}
                                                          , destination : {lat : finalState.props.destinationLat, lng : finalState.props.destinationLong, place : finalState.data.destination, address : Nothing, city : Nothing}
                                                          , sourceAddress : finalState.data.sourceAddress
                                                          , destinationAddress : finalState.data.destinationAddress })
          case finalState.props.currentStage of
            TryAgain -> do
              when (finalState.props.customerTip.enableTips) $ do
                cancelEstimate finalState.props.estimateId
              _ <- pure $ updateLocalStage TryAgain
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true}, data{nearByDrivers = Nothing}})
            _        -> do
              let sourceSpecialTagIcon = specialLocationIcons finalState.props.zoneType.sourceTag
                  destSpecialTagIcon = specialLocationIcons finalState.props.zoneType.destinationTag
              routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (Remote.normalRoute "") "NORMAL" finalState.data.source finalState.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig) 
              case rideSearchRes.routeInfo of
                Just (Route response) -> do
                  let distance = if response.distance < 1000 then toStringJSON(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
                      duration = (show (response.duration / 60)) <> " min"
                      Snapped points = response.points
                  case head points, last points of
                    Just (LatLong source), Just (LatLong dest) -> do
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : finalState.data.source, address : Nothing, city : Nothing }, destination : { lat : dest.lat, lng : dest.lon, place : finalState.data.destination, address : Nothing, city : Nothing } }) } })
                    _ , _ -> pure unit
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration,source = finalState.data.source, sourceAddress = finalState.data.sourceAddress }})
                  let distanceBtwCurrentAndSource = getDistanceBwCordinates finalState.props.sourceLat finalState.props.sourceLong finalState.props.currentLocation.lat finalState.props.currentLocation.lng
                      isDistMoreThanThreshold = (distanceBtwCurrentAndSource > finalState.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold) && flowType == "NORMAL_FLOW"
                  if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000 )then do
                    _ <- pure $ updateLocalStage DistanceOutsideLimits
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
                    else if ( (response.distance < 500  || isDistMoreThanThreshold )&& Arr.all (_ == false ) [ isLocalStageOn PickUpFarFromCurrentLocation , isLocalStageOn ShortDistance]) then do 
                      let currentStage = if isDistMoreThanThreshold then PickUpFarFromCurrentLocation else ShortDistance
                      _ <- pure $ updateLocalStage currentStage
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = currentStage ,rideRequestFlow = true, isSearchLocation = SearchLocation, isShorterTrip = response.distance < 500 , distance = response.distance, findingQuotesProgress = 0.0}})
                    else do 
                      if flowType == "REPEAT_RIDE_FLOW" then liftFlowBT $ logEventWithParams logField_ "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                      _ <- pure $ updateLocalStage FindingEstimate
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, isShorterTrip = false}, data {source = finalState.data.source, sourceAddress = finalState.data.sourceAddress, nearByDrivers = Nothing}})
                  void $ lift $ lift $ toggleLoader false
                Nothing -> pure unit
    else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{isSource = Just false, isRideServiceable = true, isSrcServiceable = true, isDestServiceable = true, currentStage = SearchLocationModel}})

  homeScreenFlow

dummyAddressGeometry :: AddressGeometry
dummyAddressGeometry = AddressGeometry {
  geometry : Geometry{
    location : LocationS{
      lat: 0.0,
      lng: 0.0
    }
  }
 }

getFinalAmount :: RideBookingRes -> Int
getFinalAmount (RideBookingRes resp) =
    let rideList = resp.rideList
        (RideAPIEntity ride) = (fromMaybe dummyRideAPIEntity (rideList !! 0))
    in INT.round $ fromMaybe 0.0 $ fromString (show (fromMaybe 0 ride.computedPrice))

tripDetailsScreenFlow :: TripDetailsGoBackType ->  FlowBT String Unit
tripDetailsScreenFlow fromMyRides = do
  (GlobalState state) <- getState
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  expiryTime <- pure $ (getExpiryTime state.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound)
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = fromMyRides, canConnectWithDriver = (expiryTime <= 86400)}, data{config = config}}) -- expiryTime < 24hrs or 86400 seconds
  flow <- UI.tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> helpAndSupportScreenFlow
    GO_TO_RIDES -> do
      (GlobalState newState) <- getState
      myRidesScreenFlow newState.myRidesScreen.props.fromNavBar
    ON_SUBMIT state -> do
      liftFlowBT $ logEventWithParams logField_ "ny_user_issue_reported" "Description" (state.data.message)
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.data.selectedItem.bookingId) state.data.message state.data.message )
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = true}})
      tripDetailsScreenFlow state.props.fromMyRides
    GO_TO_INVOICE updatedState -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_invoice_clicked" $ [ { key : "Pickup", value : unsafeToForeign updatedState.data.selectedItem.source},
                                                                                                          { key : "Destination", value : unsafeToForeign updatedState.data.selectedItem.destination},
                                                                                                          { key : "Fare", value : unsafeToForeign updatedState.data.selectedItem.totalAmount},
                                                                                                          { key : "Status", value : unsafeToForeign updatedState.data.selectedItem.status},
                                                                                                          { key : "Ride completion timestamp", value : unsafeToForeign updatedState.data.selectedItem.rideEndTime},
                                                                                                          { key : "Rating", value : (unsafeToForeign $ updatedState.data.selectedItem.rating)}]
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen = false},data{totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem, config = updatedState.data.config}})
      invoiceScreenFlow
    GO_TO_HOME state -> do
      if state.props.fromMyRides == Home then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
        updateLocalStage HomeScreen
        else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      pure $ toast (getString REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON)
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  (Just (MU.getValueFromConfig "SUPPORT_EMAIL")) (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" )
      tripDetailsScreenFlow updatedState.props.fromMyRides


invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen{data{config = config}})
  flow <- UI.invoiceScreen
  (GlobalState newState) <- getState
  case flow of
    InvoiceScreenOutput.GoBack -> tripDetailsScreenFlow newState.tripDetailsScreen.props.fromMyRides
    InvoiceScreenOutput.GoToHome -> homeScreenFlow
  pure unit

contactUsScreenFlow :: FlowBT String Unit
contactUsScreenFlow = do
  flow <- UI.contactUsScreen
  case flow of
    GO_TO_HOME_FROM_CONTACT state -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq (Just state.data.email) Nothing state.data.description state.data.subject )
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
  pure unit

helpAndSupportScreenFlow :: FlowBT String Unit
helpAndSupportScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen{data{config = config}})
  modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen{data{config = config}})
  flow <- UI.helpAndSupportScreen
  case flow of
    GO_TO_HOME_FROM_HELP -> homeScreenFlow
    GO_TO_SUPPORT_SCREEN bookingId'-> do
      modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen {data{bookingId = bookingId'}})
      contactUsScreenFlow
    GO_TO_TRIP_DETAILS state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {tripId = state.data.tripId, vehicleVariant = state.data.vehicleVariant, selectedItem {status = state.data.status, faresList = state.data.faresList ,date = state.data.date, bookingId = state.data.bookingId,rideStartTime = state.data.rideStartTime, rideEndTime = state.data.rideEndTime, rideId = state.data.rideId, vehicleNumber = state.data.vehicleNumber,time = state.data.time,source = state.data.source,destination = state.data.destination,driverName = state.data.driverName,totalAmount = state.data.totalAmount, rating = state.data.rating, shortRideId = state.data.tripId},date = state.data.date, time = state.data.time, source = state.data.source, destination = state.data.destination, driverName = state.data.driverName, totalAmount = state.data.totalAmount,rating = state.data.rating}})
      tripDetailsScreenFlow HelpAndSupport
    VIEW_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data{offsetValue = 0}})
      myRidesScreenFlow false
    UPDATE_STATE updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      helpAndSupportScreenFlow
    DELETE_USER_ACCOUNT updatedState -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq (Just updatedState.data.email) Nothing "Request To Delete Account" updatedState.data.description )
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { props{showDeleteAccountView = true}, data {accountStatus = DEL_REQUESTED}})
      helpAndSupportScreenFlow

myRidesScreenFlow :: Boolean ->  FlowBT String Unit
myRidesScreenFlow fromNavBar = do
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  (GlobalState globalState) <- getState
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen {props{fromNavBar = fromNavBar}, data{config = config, isSrcServiceable = globalState.homeScreen.props.isSrcServiceable}})
  flow <- UI.myRidesScreen
  case flow of
    REFRESH state -> myRidesScreenFlow state.props.fromNavBar
    TRIP_DETAILS state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_view_details" $ [ { key : "Pickup", value : unsafeToForeign state.data.selectedItem.source},
                                                                                                                  { key : "Destination", value : unsafeToForeign state.data.selectedItem.destination},
                                                                                                                  { key : "Fare", value : unsafeToForeign state.data.selectedItem.totalAmount},
                                                                                                                  { key : "Status", value : unsafeToForeign state.data.selectedItem.status},
                                                                                                                  { key : if state.data.selectedItem.status == "CANCELLED" then "Time" else "Ride completion timestamp",
                                                                                                                    value : unsafeToForeign $ if state.data.selectedItem.status == "CANCELLED" then state.data.selectedItem.time else state.data.selectedItem.rideEndTime},
                                                                                                                  { key : "Rating", value : (unsafeToForeign $ state.data.selectedItem.rating)}]
      modifyScreenState $ TripDetailsScreenStateType (\tripDetails -> tripDetails{data{vehicleVariant = state.data.selectedItem.vehicleVariant}})
      tripDetailsScreenFlow MyRides
    LOADER_OUTPUT state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state{data{offsetValue = state.data.offsetValue + 8}})
      myRidesScreenFlow state.props.fromNavBar
    BOOK_RIDE -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    GO_TO_NAV_BAR -> homeScreenFlow
    GO_TO_HELP_SCREEN -> helpAndSupportScreenFlow
    REPEAT_RIDE_FLOW state -> do
      updateRideDetails state
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_repeat_ride" $ [{ key : "Pickup", value : unsafeToForeign state.data.selectedItem.source},
                                                                                                                {key : "Destination", value : unsafeToForeign state.data.selectedItem.destination}]
      let sourceLat = state.data.selectedItem.sourceLocation^._lat
      let sourceLong = state.data.selectedItem.sourceLocation^._lon
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq sourceLat sourceLong)
      let srcServiceable = sourceServiceabilityResp.serviceable
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{city = sourceServiceabilityResp.city }})
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address,
                                              city : Nothing
                                            }) srcSpecialLocation.gates
      if(state.data.selectedItem.isSpecialZone) then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{sourceSelectedOnMap = false},data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson, nearByPickUpPoints = pickUpPoints}})
        pure unit
        else pure unit
      rideSearchFlow "REPEAT_RIDE_FLOW"

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
  appConfig <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> selectLanguageScreen{data{config = appConfig}})
  flow <- UI.selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
                                liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_lang_selected" $[{ key : "Previous language", value : unsafeToForeign $ getValueToLocalStore LANGUAGE_KEY},
                                                                                                                          { key : "New language", value : unsafeToForeign state.props.selectedLanguage}]
                                setValueToLocalStore LANGUAGE_KEY (state.props.selectedLanguage)
                                _ <- lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_lang_selec" "language" (state.props.selectedLanguage)
                                let langVal =  case (state.props.selectedLanguage) of
                                                                                     "HI_IN" -> "HINDI"
                                                                                     "EN_US" -> "ENGLISH"
                                                                                     "KN_IN" -> "KANNADA"
                                                                                     "BN_IN" -> "BENGALI"
                                                                                     "ML_IN" -> "MALAYALAM"
                                                                                     _ -> getValueFromConfig "defaultLanguage"
                                void $ pure $ setCleverTapUserProp [{key : "Preferred Language", value : unsafeToForeign langVal}]
                                resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
                                modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> SelectLanguageScreenData.initData)
                                homeScreenFlow
    GO_TO_HOME_SCREEN     -> homeScreenFlow

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
  flow <- UI.emergencyContactsScreen
  case flow of
    GO_TO_HOME_FROM_EMERGENCY_CONTACTS -> homeScreenFlow
    POST_CONTACTS state -> do
      _ <- Remote.emergencyContactsBT (Remote.postContactsReq state.data.contactsList)
      if state.props.showInfoPopUp then pure $ toast $ getString CONTACT_REMOVED_SUCCESSFULLY
        else pure $ toast $ getString EMERGENCY_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{props{showInfoPopUp = false}})
      (GlobalState globalState) <- getState
      if globalState.homeScreen.props.emergencyHelpModelState.isSelectEmergencyContact
      then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{emergencyHelpModelState{isSelectEmergencyContact = false, emergencyContactData = transformContactList state.data.contactsList}}})
        homeScreenFlow
      else emergencyScreenFlow
    GET_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) res.defaultEmergencyNumbers
      contactsInString <- pure $ toStringJSON contacts
      setValueToLocalStore CONTACTS (contactsInString)
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{data{contactsList = contacts}})
      emergencyScreenFlow
    REFRESH_EMERGECY_CONTACTS_SCREEN state -> do
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state)
      emergencyScreenFlow

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ AboutUsScreenStateType (\aboutUsScreen -> aboutUsScreen {appConfig = config})
  flow <- UI.aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

permissionScreenFlow :: FlowBT String Unit
permissionScreenFlow = do
  _ <- pure $ hideKeyboardOnNavigation true
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen{appConfig = config})
  flow <- UI.permissionScreen
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case flow of
    REFRESH_INTERNET -> do
        if (os == "IOS") then pure unit
          else if not internetCondition then do 
            modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = INTERNET_ACTION})
            permissionScreenFlow 
          else currentFlowStatus
    TURN_ON_GPS -> if not internetCondition then do  
                      modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = INTERNET_ACTION})
                      permissionScreenFlow
                    else do
                      setValueToLocalStore PERMISSION_POPUP_TIRGGERED "true"
                      currentFlowStatus
    TURN_ON_INTERNET -> case (getValueToLocalStore USER_NAME == "__failed") of
                            true -> pure unit
                            _ -> if os == "IOS" && not permissionConditionB then modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DENIED})
                                 else if not (permissionConditionA && permissionConditionB) then do 
                                  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                                  permissionScreenFlow 
                                 else currentFlowStatus
  pure unit

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  config <- getAppConfig Constants.appConfig
  disabilityListT <- updateDisabilityList "My_Profile_Screen"
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{data{config = config, disabilityOptions{disabilityOptionList = disabilityListT }, editedDisabilityOptions{disabilityOptionList = disabilityListT}}})
  flow <- UI.myProfileScreen
  case flow of
    UPDATE_USER_PROFILE state -> do
      _ <- pure $ toggleBtnLoader "" false
      _ <- pure $ spy "profile_updated_state" state
      let stringName = seperateByWhiteSpaces(state.data.editedName)
          name = split (Pattern " ") stringName
          nameLength = length name
          gender = getGenderValue state.data.editedGender
          email = if state.data.editedEmailId == state.data.emailId || (state.data.editedEmailId == Just "") then Nothing else state.data.editedEmailId
          disability = case state.data.editedDisabilityOptions.selectedDisability of 
            Just disability -> if (state.data.editedDisabilityOptions.activeIndex == 1) 
                                  then Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.editedDisabilityOptions.otherDisabilityReason))
                                  else Nothing
            _ -> Nothing
          hasDisability = if state.props.changeAccessibility then (Just (isJust disability)) else Nothing
      resp <- if nameLength > 2 then
                lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender hasDisability disability)
                else if nameLength == 2 then
                  lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender hasDisability disability)
                  else if nameLength == 1 then
                    lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender hasDisability disability)
                    else
                      lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender hasDisability disability)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME stringName
          let tag = case disability of
                      Just (Disability value) -> value.tag
                      Nothing -> ""
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{ disability = Just {id : "", tag : tag, description : "" }}})
          case gender of
            Just gender -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just gender}}, props{isBanner = false}})
            _ -> pure unit
          case email of
            Just email -> do
              setValueToLocalStore USER_EMAIL email
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just email}}})
            _ -> pure unit
          modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData)
          myProfileScreenFlow
        Left err -> do
          let errResponse = err.response
          let codeMessage = decodeError errResponse.errorMessage "errorCode"
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              pure $ setText (getNewIDWithTag "EmailEditText") ""
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{props{isEmailValid = false, updateProfile = true}, data{emailErrorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender}})
            _ -> pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          myProfileScreenFlow
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

savedLocationFlow :: FlowBT String Unit
savedLocationFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ SavedLocationScreenStateType (\savedLocationScreen -> savedLocationScreen{data{config = config}})
  void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  flow <- UI.savedLocationScreen
  (SavedLocationsListRes savedLocationResp )<- Remote.getSavedLocationBT SavedLocationReq
  case flow of
    ADD_NEW_LOCATION state-> do
      (GlobalState newState) <- getState
      resp <- lift $ lift $ getRecentSearches newState.addNewAddressScreen
      let recents = map
                    (\item -> item{postfixImageUrl = "", postfixImageVisibility = false}) (differenceOfLocationLists (resp.predictionArray) ((AddNewAddress.getSavedLocations savedLocationResp.list)))
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{savedLocations = getSavedLocationForAddNewAddressScreen state.data.savedLocations ,locationList = recents ,placeName = "",address = "",addressSavedAs="", recentSearchs{predictionArray = recents},savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)}, props{showSavePlaceView = false, editLocation = false, isLocationServiceable = true, isSearchedLocationServiceable = true, isLocateOnMap = false, fromHome = false}})
      case (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "HOME" ""), (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "WORK" "") of
          false   , false    -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = (Just 2), selectedTag = (Just OTHER_TAG) }, props{editSavedLocation = false}})
          _ , _ -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = Nothing, selectedTag = Nothing}, props{editSavedLocation = false}})
      addNewAddressScreenFlow "dummy"
    DELETE_LOCATION tagName -> do
      resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim tagName))
      pure $ toast (getString FAVOURITE_REMOVED_SUCCESSFULLY)
      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      savedLocationFlow
    EDIT_LOCATION cardState -> do
      (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq (fromMaybe 0.0 cardState.lat) (fromMaybe 0.0 cardState.lon))
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
        addNewAddressScreen
          { props
              { tagExists = false
              , showSavePlaceView = true
              , editLocation= true
              , editSavedLocation = true
              , isBtnActive = false
              , isLocateOnMap = false
              , isLocationServiceable = (serviceabilityRes.serviceable)
              , fromHome = false
              }
          , data
              { existsAs = ""
              , selectedTag = getCardType (fromMaybe "" (cardState.cardType))
              , placeName = cardState.tagName
              , savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list)
              , address = cardState.savedLocation
              , addressSavedAs = cardState.tagName
              , selectedItem
                  { title = (fromMaybe "" ((split (Pattern ",") (cardState.savedLocation)) !! 0))
                  , description = cardState.savedLocation
                  , lat = cardState.lat
                  , lon = cardState.lon
                  , placeId = cardState.placeId
                  , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (cardState.savedLocation))) + 2) (cardState.savedLocation))
                  }
              , savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)
              , lat = fromMaybe 0.0 cardState.lat
              , lon = fromMaybe 0.0 cardState.lon
              , latSelectedFromMap = fromMaybe 0.0 cardState.lat
              , lonSelectedFromMap = fromMaybe 0.0 cardState.lon
              , locSelectedFromMap = ""
              , activeIndex = case (getCardType (fromMaybe "" (cardState.cardType))) of
                                Just card -> case card of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                OTHER_TAG-> Just 2
                                Nothing   -> Nothing}})

      addNewAddressScreenFlow "edit Location"

    GO_BACK_FROM_SAVED_LOCATION -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow
  pure unit

addNewAddressScreenFlow ::String -> FlowBT String Unit
addNewAddressScreenFlow input = do
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{config = config}})
  flow <- UI.addNewAddressScreen
  case flow of
    SEARCH_ADDRESS input state -> do
      (GlobalState newState) <- getState
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( newState.homeScreen.props.sourceLat) ( newState.homeScreen.props.sourceLong) getSearchRadius (EHC.getMapsLanguageFormat (getValueToLocalStore LANGUAGE_KEY) ) "")
      let sortedByDistanceList = sortPredctionByDistance searchLocationResp.predictions
          predictionList = AddNewAddress.getLocationList sortedByDistanceList
          recentLists = state.data.recentSearchs.predictionArray
          filteredRecentsList = filterRecentSearches recentLists predictionList
          filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> state{  data  { locationList = map
                                                                                                                (\item -> item{ postfixImageVisibility = (not (checkPrediction item state.data.savedLocations))
                                                                                                                              , postfixImageUrl = if (checkPrediction item state.data.savedLocations) then "" else fetchImage FF_ASSET "ny_ic_fav_red"
                                                                                                                              , isClickable = (checkPrediction item state.data.savedLocations)
                                                                                                                              , alpha = if (checkPrediction item state.data.savedLocations) then 1.0 else 0.5 }) (filteredPredictionList <> filteredRecentsList) }})
      addNewAddressScreenFlow ""

    ADD_LOCATION state -> do
      if (state.props.editSavedLocation) then do
        _ <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim state.data.placeName))
        pure unit
      else pure unit
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_favourite_added" $ [{ key : "Address", value : unsafeToForeign state.data.address},
                                                                                                                { key : "Tag", value : unsafeToForeign state.data.selectedTag}]
      (GetPlaceNameResp sourcePlace) <- getPlaceNameResp (state.data.selectedItem.title <> ", " <> state.data.selectedItem.subTitle) state.data.selectedItem.placeId (fromMaybe 0.0 state.data.selectedItem.lat) (fromMaybe 0.0 state.data.selectedItem.lon)  state.data.selectedItem
      let source = state.data.selectedItem.description
          (PlaceName sourceAddressGeometry) = (fromMaybe HomeScreenData.dummyLocationName (sourcePlace!!0))
          (LatLong sourceLocation) = (sourceAddressGeometry.location)
          lat = sourceLocation.lat
          lng = sourceLocation.lon
          newstate = state { data { lat =lat, lon=lng, selectedItem
                                                        { description = source
                                                        , lat = Just lat
                                                        , lon = Just lng
                                                        }
                                    , addressComponents = sourceAddressGeometry.addressComponents
                                    }
                              }

      resp <- Remote.addSavedLocationBT (AddNewAddress.encodeAddressDescription newstate)
      if state.props.editSavedLocation then pure $ toast (getString FAVOURITE_UPDATED_SUCCESSFULLY)
        else pure $ toast (getString FAVOURITE_ADDED_SUCCESSFULLY)

      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      if state.props.fromHome then do
        (GlobalState globalState) <- getState
        (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
        case savedLocationResp of
          Right (SavedLocationsListRes listResp) -> do
            let updatedLocationList = getUpdatedLocationList globalState.homeScreen.data.locationList state.data.selectedItem.placeId
            modifyScreenState $ HomeScreenStateType (\homeScreen ->
                                                        homeScreen
                                                          { data
                                                              { settingSideBar {opened = SettingSideBarController.CLOSED, appConfig = DC.config}
                                                              , locationList = updatedLocationList
                                                              , savedLocations = (AddNewAddress.getSavedLocations listResp.list)
                                                              }
                                                            } )

            homeScreenFlow
          Left (err) -> homeScreenFlow
        else savedLocationFlow

    UPDATE_LOCATION_NAME_ADDRESS state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let isServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address,
                                              city : Nothing
                                            }) srcSpecialLocation.gates
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      if (fromMaybe "" sourceServiceabilityResp.geoJson) /= "" && (fromMaybe "" sourceServiceabilityResp.geoJson) /= state.data.polygonCoordinates && pickUpPoints /= state.data.nearByPickUpPoints then do
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data { polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson
                                                                                                             , nearByPickUpPoints = pickUpPoints
                                                                                                             }
                                                                                                      , props{ isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing
                                                                                                             , isServiceable = isServiceable
                                                                                                             }
                                                                                                      })
        _ <- pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = (fromMaybe "" sourceServiceabilityResp.geoJson), points = pickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "AddAddressPin"}
        addNewAddressScreenFlow ""
      else do
        PlaceName address <- getPlaceName lat lon gateAddress
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data  { locSelectedFromMap = address.formattedAddress
                                                                                                              , latSelectedFromMap = lat
                                                                                                              , lonSelectedFromMap = lon
                                                                                                              }
                                                                                                      , props { isServiceable = isServiceable }
                                                                                                      } )
        addNewAddressScreenFlow ""
    GO_TO_FAVOURITES -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      savedLocationFlow

    CHECK_LOCATION_SERVICEABILITY state locItemType-> do
      _ <- pure $ spy "Inside CHECK_LOCATION_SERVICEABILITY" state
      let item  = state.data.selectedItem
      if item.locationItemType /= Just RECENTS then do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) item
        let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
        let (LatLong placeLatLong) = (placeName.location)
        (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon)
        case (serviceabilityRes.serviceable) , (state.props.editLocation) of
          true , isEditLocation ->  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if isEditLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case isEditLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = isEditLocation
                  }
                } )
          _    ,  _     -> do
            pure $ setText (getNewIDWithTag "SavedLocationEditText") item.description
            _ <- pure $ hideKeyboardOnNavigation true
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { isSearchedLocationServiceable = false
                  , isLocateOnMap = false
                  , showSavePlaceView = false
                  }
                , data
                  { recentSearchs { predictionArray = state.data.recentSearchs.predictionArray }
                  , address = item.description
                  }
                } )
            addNewAddressScreenFlow ""
        updateDistanceInfo state (Just placeLatLong.lat) (Just placeLatLong.lon)
      else do
        let recentItem = (fromMaybe dummyLocationListItemState ( (filter (\ ( recent) -> (recent.placeId) == (item.placeId))(state.data.recentSearchs.predictionArray)) !! 0))
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if state.props.editLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case state.props.editLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = state.props.editLocation
                  }
                } )
        updateDistanceInfo state recentItem.lat recentItem.lon
    GO_TO_HOME_SCREEN_FLOW -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow

  pure unit



referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { config = config })
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{referralCode = (Just referralCode)}
      res <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case res of
        Right response -> do
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true })
          setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{isReferred = true} })
        Left err -> do
          if ((err.code == 500 && (decodeError err.response.errorMessage "errorCode") == "BPP_INTERNAL_API_ERROR")) then
            modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { isInvalidCode = true })
          else do
            _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
            pure unit
      referralScreenFlow
    BACK_TO_HOME -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> ReferralScreen.initData)
      _ <- lift $ lift $ liftFlow $ adjustViewWithKeyboard "true"
      homeScreenFlow

drawDottedRoute :: HomeScreenState -> FlowBT String Unit
drawDottedRoute state = do
  _ <- pure $ removeAllPolylines ""
  let destMarker = if state.props.currentStage == RideAccepted then "src_marker" else "dest_marker"
      srcMarker = "ny_ic_auto_map"
      srcLat = state.data.driverInfoCardState.driverLat
      srcLng = state.data.driverInfoCardState.driverLng
      destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
      destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
  lift $ lift $ liftFlow $ drawRoute (Remote.walkCoordinate srcLat srcLng destLat destLng) "DOT" "#323643" false srcMarker destMarker 8 "DRIVER_LOCATION_UPDATE" "" "" (specialLocationConfig "" "" false getPolylineAnimationConfig) 

isForLostAndFound :: Boolean
isForLostAndFound = true



checkAndUpdateSavedLocations :: HomeScreenState -> FlowBT String Unit
checkAndUpdateSavedLocations state = do
  if (getValueToLocalStore RELOAD_SAVED_LOCATION == "true") || (state.props.currentStage == HomeScreen)
    then do
      recentPredictionsObject <- lift $ lift $ getObjFromLocal state
      (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_home_blue" || (listItem.prefixImageUrl == fetchImage FF_ASSET "ny_ic_work_blue"))) (AddNewAddress.getSavedLocations listResp.list))
          let recent = (differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag)
          let twoElements = catMaybes ([] <> [recent!!0] <> [recent!!1])
          setValueToLocalStore RELOAD_SAVED_LOCATION "false"
          modifyScreenState $
            HomeScreenStateType
              (\homeScreen ->
                homeScreen
                {
                  data
                  { recentSearchs
                    { predictionArray =
                        map
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then fetchImage FF_ASSET "ny_ic_fav_red" else fetchImage FF_ASSET "ny_ic_fav" } ) twoElements},
                      savedLocations = (AddNewAddress.getSavedLocations listResp.list),
                      locationList =  map
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then fetchImage FF_ASSET "ny_ic_fav_red" else fetchImage FF_ASSET "ny_ic_fav" } ) recent
                    }
                  }
                )
          pure unit
        Left (err) -> pure unit
      pure unit
    else pure unit

addLocationToRecents :: LocationListItemState -> HomeScreenState -> Boolean -> Boolean -> FlowBT String Unit
addLocationToRecents item state srcServiceable destServiceable = do
  let serviceable = if (state.props.isSource == Just false) then destServiceable else srcServiceable
  case item.locationItemType of
    Just PREDICTION -> do
        let lat = if (state.props.isSource == Just false) then state.props.destinationLat else state.props.sourceLat
        let lon = if (state.props.isSource == Just false) then state.props.destinationLong else state.props.sourceLong
        saveToRecents item lat lon serviceable
    _ -> saveToRecents item (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) serviceable
  pure unit

saveToRecents :: LocationListItemState -> Number -> Number -> Boolean -> FlowBT String Unit
saveToRecents item lat lon serviceability = do
  (GlobalState currentState) <- getState
  recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
  if serviceability then do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{recentSearchs{predictionArray = addToRecentSearches item{lat = Just lat, lon = Just lon} recentPredictionsObject.predictionArray}, locationList = ((addToRecentSearches item{lat = Just lat, lon = Just lon} recentPredictionsObject.predictionArray) )}})
    (GlobalState modifiedState) <- getState
    _ <- pure $ saveObject "RECENT_SEARCHES" modifiedState.homeScreen.data.recentSearchs
    pure unit
    else pure unit
  pure unit


addLocToCurrLoc :: Number -> Number -> String -> FlowBT String Unit
addLocToCurrLoc lat lon name = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations{ pastCurrentLocations = addToPrevCurrLoc {lat: lat, lon:lon, placeName : name} homeScreen.data.previousCurrentLocations.pastCurrentLocations}}})
  (GlobalState modifiedState) <- getState
  _ <- pure $ saveCurrentLocations "PREVIOUS_CURRENT_LOCATION" modifiedState.homeScreen.data.previousCurrentLocations
  pure unit

getDistanceInfo :: Array LocationListItemState -> String -> Number -> Number -> String -> FlowBT String {tagExists :: Boolean, locExistsAs :: String }
getDistanceInfo savedLocations excludeLocation lat lon placeId = do
  distArr <- pure $ ((AddNewAddress.calculateDistance savedLocations excludeLocation lat lon))
  rslt <- pure $ ((AddNewAddress.isValidLocation savedLocations excludeLocation placeId))
  let placeIdExists =(fromMaybe {locationName : "" , distanceDiff : 1.0} ((rslt)!!0))
      minDist = ((fromMaybe {locationName : "" , distanceDiff : 1.0} ((distArr)!!0)))
      locExistsAs = case placeIdExists.locationName /= "" , minDist.distanceDiff <= 0.020 of
                      true , _ -> placeIdExists.locationName
                      false    , true -> minDist.locationName
                      _ , _ -> ""
      tagExists = ((length rslt) > 0 || minDist.distanceDiff <= 0.020)
  pure $ { tagExists, locExistsAs }



updateDistanceInfo :: AddNewAddressScreenState ->Maybe Number ->Maybe Number -> FlowBT String Unit
updateDistanceInfo state lat lon = do

            distanceInfo <- getDistanceInfo state.data.savedLocations  (if state.props.editLocation then state.data.placeName else "") (fromMaybe 0.0 lat) (fromMaybe 0.0 lon) (fromMaybe "" state.data.selectedItem.placeId)
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { tagExists = distanceInfo.tagExists
                  , isLocateOnMap = false
                  , showSavePlaceView = true
                  , isBtnActive = case state.data.activeIndex of
                                    Just 2 -> if state.data.addressSavedAs /= "" then true else false
                                    Just index -> true
                                    Nothing -> false
                  }
                , data
                  { selectedTag = state.data.selectedTag
                  , activeIndex = state.data.activeIndex
                  , existsAs = distanceInfo.locExistsAs
                  }
                } )
            addNewAddressScreenFlow ""

dummyLocationListItemState :: LocationListItemState
dummyLocationListItemState = dummyLocationListState{locationItemType = Just PREDICTION}

removeChatService :: String -> FlowBT String Unit -- TODO:: Create a chat service and remove this
removeChatService _ = do
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit

setFlowStatusData :: Encode FlowStatusData => FlowStatusData -> Effect Unit
setFlowStatusData object = void $ pure $ setValueToLocalStore FLOW_STATUS_DATA (encodeJSON object)

updateFlowStatus :: NotifyFlowEventType -> FlowBT String Unit
updateFlowStatus eventType = do
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  case flowStatus.currentStatus of
    RIDE_ASSIGNED _ -> do
      checkRideStatus true
      homeScreenFlow
    _               -> do
      res <- lift $ lift $ Remote.notifyFlowEvent (Remote.makeNotifyFlowEventReq (show eventType))
      case res of
        Right _  -> homeScreenFlow
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorCode"
          when ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") $ do
            void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
            checkRideStatus false

getTicketBookings :: Array TicketBookingItem -> Array TicketBookingItem -> TicketBookings
getTicketBookings bookedRes pendingRes = {
  pendingBooking : pendingRes,
  booked : bookedRes
}


cancelEstimate :: String -> FlowBT String Unit
cancelEstimate bookingId = do
  logField_ <- lift $ lift $ getLogFields
  res <- lift $ lift $ Remote.cancelEstimate bookingId
  if bookingId == ""
    then do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = HomeScreen}})
    else do
      case res of
        Right res -> do
          -- TODO : to be removed after new bundle is 100% available (replace with pure unit)
          let (CancelEstimateRes resp) = res
          case resp.result of
            "Success" -> do
              if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
                _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_driver_assign"
                pure unit
                else do
                  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_quotes"
                  pure unit
            "BookingAlreadyCreated" -> do
              void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
              _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_right"
              checkRideStatus true
              homeScreenFlow
            _ -> do
              void $ pure $ toast $ getString CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
              _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_right"
              homeScreenFlow
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorCode"
          if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
            void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
            _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_left"
            checkRideStatus true
            homeScreenFlow
          else do
            void $ pure $ toast $ getString CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
            _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_left"
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = HomeScreen}}) 
            homeScreenFlow

getGenderValue :: Maybe Gender.Gender -> Maybe String
getGenderValue gender =
  case gender of
    Just value -> case value of
      Gender.MALE -> Just "MALE"
      Gender.FEMALE -> Just "FEMALE"
      Gender.OTHER -> Just "OTHER"
      _ -> Just "PREFER_NOT_TO_SAY"
    Nothing -> Nothing

getPlaceCoordinates :: String -> FlowBT String { latitude :: Number, longitude :: Number }
getPlaceCoordinates address =
  let {latitude, longitude} = runFn1 getLatLonFromAddress address
  in pure {latitude, longitude}

getPlaceName :: Number -> Number -> Location -> FlowBT String PlaceName
getPlaceName lat long location = do
  case location.address of
    Just address -> do
      let addressComponent = mkAddressComponent location "sublocality"
      pure $ mkPlaceName lat long address (Just addressComponent)
    Nothing -> do
      let address = runFn2 getLocationNameV2 lat long
      config <- getAppConfig Constants.appConfig
      logField_ <- lift $ lift $ getLogFields
      if address /= "NO_LOCATION_FOUND" && config.geoCoder.enableLLtoAddress then do
        liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_found"
        pure $ mkPlaceName lat long address Nothing
      else do
        (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat long $ EHC.getMapsLanguageFormat $ getValueToLocalStore LANGUAGE_KEY)
        liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_fallback"
        pure $ (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
  where 
    mkPlaceName :: Number -> Number -> String -> Maybe AddressComponents -> PlaceName
    mkPlaceName lat long address addressComponent = 
      PlaceName {
        formattedAddress : address
      , location : LatLong { lat : lat, lon : long }
      , plusCode : Nothing
      , addressComponents : [] <> catMaybes [addressComponent]
      }
    mkAddressComponent :: Location -> String -> AddressComponents
    mkAddressComponent location addressType =
      AddressComponents {
          longName : location.place
        , shortName : location.place
        , types : [addressType]
      }

dummyLocationData :: LocationData
dummyLocationData = LocationData {
    lat : 0.0
  , lon : 0.0
  , name : Nothing
}

checkAndUpdateLocations :: FlowBT String Unit
checkAndUpdateLocations = do
  payload <- liftFlowBT $ getGlobalPayload unit
  case payload of
    Just (GlobalPayload payload') -> do
      _ <- pure $ spy "inside right" payload'
      let (Payload innerPayload) = payload'.payload
      case isNothing innerPayload.search_type of
        true -> pure unit
        false -> do
          let searchType = fromMaybe "normal_search" $ innerPayload.search_type
          if searchType /= "normal_search" then do
            let (LocationData source) = fromMaybe dummyLocationData innerPayload.source
            let (LocationData destination) = fromMaybe dummyLocationData innerPayload.destination
            modifyScreenState $ HomeScreenStateType (\homescreen -> homescreen {
              data {
                source = (fromMaybe "" source.name)
              , destination = (fromMaybe "" destination.name)
              , sourceAddress = encodeAddress (fromMaybe "" source.name) [] Nothing
              , destinationAddress = encodeAddress (fromMaybe "" destination.name) [] Nothing
              }, props {
                  sourceLat = source.lat
                , sourceLong = source.lon
                , destinationLat = destination.lat
                , destinationLong = destination.lon
                , isSource = Just false
                , isSearchLocation = SearchLocation
              }
            })
          else pure unit
    Nothing ->do
      _ <- pure $ spy "inside left" "err"
      pure unit

rideCompletedDetails :: RideBookingRes -> Array ClevertapEventParams
rideCompletedDetails (RideBookingRes resp) = do
  let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
      (RideBookingDetails contents) = bookingDetails.contents
      (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
      differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
      finalAmount =  getFinalAmount (RideBookingRes resp)
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)

  [ {key : "Estimate ride distance (km)", value : unsafeToForeign (fromMaybe 0 contents.estimatedDistance/1000)},
          {key : "Actual ride distance (km)", value : unsafeToForeign ((fromMaybe 0 ride.chargeableRideDistance)/1000)},
          {key : "Difference between estimated and actual ride distance (km)" , value : unsafeToForeign (differenceOfDistance/1000)},
          {key : "Total Estimated fare (₹)",value : unsafeToForeign (resp.estimatedFare)},
          {key : "Total Actual fare (₹)",value : unsafeToForeign (finalAmount)},
          {key : "Difference between estimated and actual fares (₹)",value : unsafeToForeign (resp.estimatedFare - finalAmount)},
          {key : "Driver pickup charges (₹)",value : unsafeToForeign "10"},
          {key : "Night ride",value : unsafeToForeign nightChargesVal}]

personStatsData :: PersonStatsRes -> GetProfileRes -> Array ClevertapEventParams
personStatsData (PersonStatsRes resp) (GetProfileRes response) = [{key : "First ride taken" , value : unsafeToForeign if response.hasTakenRide then "true" else "false"},
                                                                  {key : "Common App Use Case",value : unsafeToForeign resp.commonAppUseCase},
                                                                  {key : "Emergency Contacts Num", value : unsafeToForeign resp.emergencyContactsNum },
                                                                  {key : "Favourite Locations Num", value : unsafeToForeign resp.favoriteLocationsNum},
                                                                  {key : "Frequency Category" , value : unsafeToForeign resp.frequencyCategory},
                                                                  {key : "Is Blocked", value : unsafeToForeign resp.isBlocked},
                                                                  {key : "Is Churned User", value : unsafeToForeign resp.isChurnedUser},
                                                                  {key : "Is WhatsApp Opt-In Status" , value : unsafeToForeign resp.isWhatsAppOptInStatus},
                                                                  {key : "total_rider_trips" , value : unsafeToForeign resp.lifetimeRides},
                                                                  {key : "Last Ride Taken" , value : unsafeToForeign (fromMaybe "" resp.lastRideTaken)},
                                                                  {key : "Latest Search", value : unsafeToForeign (fromMaybe "" resp.latestSearch)},
                                                                  {key : "Off Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.offPeakRidesRate)},
                                                                  {key : "Overall Cancellation Rate", value : unsafeToForeign (fromMaybe 0.0 resp.overalCancellationRate)},
                                                                  {key : "Sign-up Date", value : unsafeToForeign resp.signupDate},
                                                                  {key : "Status" , value : unsafeToForeign (fromMaybe "" resp.status)},
                                                                  {key : "User Cancellation Rate", value : unsafeToForeign (fromMaybe 0.0 resp.userCancellationRate)},
                                                                  {key : "User Category", value : unsafeToForeign resp.userCategory},
                                                                  {key : "Weekday Evening Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayEveningPeakRidesRate)},
                                                                  {key : "Weekday Morning Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayMorningPeakRidesRate)},
                                                                  {key : "Weekday Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayRidesRate)},
                                                                  {key : "Weekend Peak Ride Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekendPeakRideRate)},
                                                                  {key : "Weekend Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekendRidesRate)}
                                                                  ]

updateSourceLocation :: String ->  FlowBT String Unit
updateSourceLocation _ = do 
  (GlobalState currentState) <- getState
  let disabled = case currentState.homeScreen.data.disability of 
                      Just val -> Just val.tag
                      Nothing -> Just ""
  when (disabled == Just "BLIND_LOW_VISION" ) $ do
    PlaceName address <- getPlaceName currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong HomeScreenData.dummyLocation
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data{ source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing } })
    pure unit
  pure unit 

updateUserInfoToState :: HomeScreenState -> FlowBT String Unit
updateUserInfoToState state =
  modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{disability = state.data.disability, settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props { isBanner = state.props.isBanner}})             

zooTicketBookingFlow :: FlowBT String Unit
zooTicketBookingFlow = do
  liftFlowBT $ hideLoader
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})             
  flow <- UI.ticketBookingScreen
  case flow of
    GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
    GET_BOOKING_INFO_SCREEN state bookingStatus -> do
      (GetBookingInfoRes resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId--state.props.selectedBookingInfo.shortId (show state.props.selectedBookingInfo.status)
      if (bookingStatus == Pending)
        then do
          modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
          setValueToLocalStore PAYMENT_STATUS_POOLING "true"
          fillBookingDetails (GetBookingInfoRes resp) state.props.selectedBookingId "Pending"
          zooTicketBookingFlow
        else do
          let ticketBookingDetails = (ticketDetailsTransformer (GetBookingInfoRes resp))
          let dummyListItem = { ticketServiceShortId : "", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,attendeeType: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,attendeeType: "Mobile"}]}
          modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
          zooTicketInfoFlow
    GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      homeScreenFlow
    RESET_SCREEN_STATE -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      zooTicketBookingFlow
    REFRESH_PAYMENT_STATUS state -> do
      (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
      updatePaymentStatusData ticketStatus state.props.selectedBookingId
      setValueToLocalStore PAYMENT_STATUS_POOLING "false"
      zooTicketBookingFlow

ticketPaymentFlow :: TicketBookingScreenData -> FlowBT String Unit
ticketPaymentFlow screenData = do
  liftFlowBT $ initiatePaymentPage
  (CreateOrderRes orderResp) <- Remote.bookTicketsBT (Remote.mkBookingTicketReq screenData) ticketPlaceId
  let (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
      (PayPayload innerpayload) = sdk_payload.payload
      finalPayload = PayPayload $ innerpayload{ language = Just (getPaymentPageLangKey (getValueToLocalStore LANGUAGE_KEY)) }
      sdkPayload = PaymentPagePayload $ sdk_payload{payload = finalPayload}
      shortOrderID = orderResp.order_id
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{shortOrderId = shortOrderID}, props{selectedBookingId = shortOrderID}})
  lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
  _ <- paymentPageUI sdkPayload
  void $ lift $ lift $ toggleLoader true
  _ <- pure $ toggleBtnLoader "" false
  (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT shortOrderID
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
  updatePaymentStatusData ticketStatus shortOrderID
  void $ lift $ lift $ toggleLoader false
  zooTicketBookingFlow

updatePaymentStatusData :: String -> String -> FlowBT String Unit
updatePaymentStatusData ticketStatus shortOrderID =
  case ticketStatus of
    "Booked" -> do
      infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
      fillBookingDetails infoRes shortOrderID ticketStatus
    "Pending" -> do
      _ <- pure $ toast $ "Fetching the status"
      infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
      setValueToLocalStore PAYMENT_STATUS_POOLING "true"
      fillBookingDetails infoRes shortOrderID ticketStatus
    "Failed" -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { paymentStatus = Common.Failed } })
    _ -> do
      _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = ticketBookingScreen.props.previousStage} }) -- temporary fix - will remove once 500 INTERNAL_SERVER_ERROR is solved.
      pure unit

zooTicketInfoFlow :: FlowBT String Unit
zooTicketInfoFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.ticketInfoScreen
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState ->  ticketBookingScreenState{props{currentStage = MyTicketsStage}})
  case flow of
    GO_TO_HOME_SCREEN_FROM_TICKET_INFO -> currentFlowStatus
    _ -> pure unit

fillBookingDetails :: GetBookingInfoRes -> String -> String -> FlowBT String Unit
fillBookingDetails (GetBookingInfoRes resp) shortOrderID ticketStatus = do
  let
    serv = resp.services !! 0
  modifyScreenState
    $ TicketBookingScreenStateType
        ( \ticketBookingScreen ->
            ticketBookingScreen
              { props
                { paymentStatus = if ticketStatus == "Booked" then Common.Success else Common.Pending
                }
              , data
                { zooName = resp.ticketPlaceName
                , keyValArray =
                  [ { key: "Date", val: resp.visitDate }
                  , { key: "Booking For", val: "" }
                  , { key: "Total Paid", val: ("₹" <> show resp.amount) }
                  , { key: "Booking ID", val: resp.ticketShortId }
                  , { key: "Transaction ID", val: shortOrderID }
                  ]
                    <> case serv of
                        Nothing -> []
                        Just (TicketBookingServiceDetails serviceDetails) -> if isJust serviceDetails.expiryDate then [ { key: "Valid until", val: (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "hh:mm A") <> ", " <> (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "Do MMM YYYY") } ] else []
                , bookedForArray = (map (\(TicketBookingServiceDetails item) -> item.ticketServiceName) resp.services)
                }
              }
        )

dummyTicketPlaceResp :: TicketPlaceResp -- TODO:: Temp done for testing, remove after the release
dummyTicketPlaceResp = TicketPlaceResp
  { id : "",
    merchantOperatingCityId : "",
    name : "",
    description : Nothing,
    lat : Nothing,
    lon : Nothing,
    gallery : [],
    openTimings : Nothing,
    closeTimings : Nothing
  }

checkForSpecialZoneAndHotSpots :: HomeScreenState -> ServiceabilityRes -> Number -> Number -> FlowBT String Unit
checkForSpecialZoneAndHotSpots state (ServiceabilityRes serviceabilityResp) lat lon = do
  let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation serviceabilityResp.specialLocation
  let pickUpPoints = map (\(GatesInfo item) -> {  place: item.name,
                                                  lat  : (item.point)^._lat,
                                                  lng : (item.point)^._lon,
                                                  address : item.address,
                                                  city : Nothing
                                               }) srcSpecialLocation.gates
  let geoJson = fromMaybe "" serviceabilityResp.geoJson
  if not (DS.null geoJson) && not (null pickUpPoints) then do
    _ <- pure $ spy "debug hotspot cond 1" "."
    when (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) $ do
      _ <- pure $ spy "debug hotspot cond 2" "."
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = geoJson, nearByPickUpPoints = pickUpPoints},props{city = serviceabilityResp.city, isSpecialZone =  not (DS.null geoJson) , confirmLocationCategory = srcSpecialLocation.category}})
      _ <- pure $ removeAllPolylines ""
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = (fromMaybe "" serviceabilityResp.geoJson), points = pickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
      homeScreenFlow
  else if not (null serviceabilityResp.hotSpotInfo) then do
    _ <- pure $ spy "debug hotspot cond 4" "."
    let hotSpotInfo = spy "debug hotspot data" $ transformHotSpotInfo (filter (\(HotSpotInfo hotSpot) ->
                                                                                  let (LatLong point) = hotSpot.centroidLatLong
                                                                                      distance = (getDistanceBwCordinates lat lon point.lat point.lon) * 1000.0
                                                                                  in
                                                                                    distance < 100.0
                                                                              ) serviceabilityResp.hotSpotInfo)
    let points = map (\item -> {  place: "",
                                    lat  : item.lat,
                                    lng : item.lon,
                                    address : Nothing,
                                    city : Nothing
                                  }) hotSpotInfo
    when (state.data.nearByPickUpPoints /= points && not (null points)) $ do
      _ <- pure $ spy "debug hotspot cond 5" "."
      pure $ removeAllPolylines ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = "", nearByPickUpPoints = points},props{isSpecialZone = false, defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (points!!0)).place, confirmLocationCategory = srcSpecialLocation.category}})
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = "", points = points, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
      -- homeScreenFlow
  else do
    _ <- pure $ spy "debug hotspot cond 7" "."
    pure unit