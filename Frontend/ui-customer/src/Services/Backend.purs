{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Backend where

import Services.API

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..))
import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck (..))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Engineering.Helpers.Commons (liftFlow, os, bundleVersion)
import Foreign.Generic (encode)
import Helpers.Utils (decodeErrorCode, decodeErrorMessage, toString, getTime, getPreviousVersion)
import JBridge (Locations, factoryResetApp, setKeyInSharedPrefKeys, toast, drawRoute, toggleBtnLoader)
import Juspay.OTP.Reader as Readers
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Screens.Types (AccountSetUpScreenState(..), HomeScreenState(..), NewContacts)
import Types.App (GlobalState(..), FlowBT, ScreenType(..))
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Presto.Core.Types.API (Header(..), Headers(..), ErrorResponse)
-- import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorPayload, Method(..), defaultDecodeResponse, defaultMakeRequest, standardEncode)
import Presto.Core.Types.Language.Flow (Flow, callAPI, doAff, loadS)
import Screens.Types (Address, Stage(..))
import JBridge (factoryResetApp, setKeyInSharedPrefKeys, toast, removeAllPolylines, stopChatListenerService, SpecialLocationTag)
import Prelude (Unit, bind, discard, map, pure, unit, void, ($), ($>), (&&), (*>), (<<<), (=<<), (==), (<=),(||), show, (<>))
import Storage (getValueToLocalStore, deleteValueFromLocalStore, getValueToLocalNativeStore, KeyStore(..), setValueToLocalStore)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Types.App (GlobalState, FlowBT, ScreenType(..))
import Types.EndPoint as EP
import Engineering.Helpers.Commons (liftFlow, os, bundleVersion, isPreviousVersion)
import Data.Array ((!!), take)
import Language.Strings (getString)
import Language.Types (STR(..))
import Services.Config as SC
import Engineering.Helpers.Utils as EHU
import MerchantConfig.Utils (Merchant(..), getMerchant)

getHeaders :: String -> Boolean -> Flow GlobalState Headers
getHeaders _ isGzipCompressionEnabled = do
    regToken <- loadS $ show REGISTERATION_TOKEN
    pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                      <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []

getHeaders' :: String -> Boolean -> FlowBT String Headers
getHeaders' _ isGzipCompressionEnabled = do
    regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
    lift $ lift $ pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                      <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []



----------------------------------------------------------- API Results & BT Functions-------------------------------------------------------------------------------------------------

withAPIResult url f flow = do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResult url" url
    case resp of
        Right res -> do
            _ <- (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "") $> res
            pure unit
        Left (err) -> do
            _ <- pure $ toggleBtnLoader "" false
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err
            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage

            _ <- (trackApiCallFlow Tracker.Network Tracker.Exception DETAILS start end (err.code) (codeMessage) url "" "") $> errResp
            _ <- trackExceptionFlow Tracker.API_CALL Tracker.Sdk DETAILS url (codeMessage)
            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
                _ <- pure $ deleteValueFromLocalStore LANGUAGE_KEY
                _ <- pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
                _ <- liftFlow $ stopChatListenerService
                _ <- pure $ factoryResetApp ""
                pure unit -- default if it fails
                else pure unit -- default if it fails
    pure resp

withAPIResultBT url f errorHandler flow = do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResultBT url" url
    case resp of
        Right res -> do
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
        Left (err) -> do
            _ <- pure $ toggleBtnLoader "" false
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err

            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage
            _ <- pure $ printLog "message" userMessage
            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                deleteValueFromLocalStore REGISTERATION_TOKEN
                deleteValueFromLocalStore LANGUAGE_KEY
                deleteValueFromLocalStore REGISTRATION_APPROVED
                lift $ lift $ liftFlow $ stopChatListenerService
                pure $ factoryResetApp ""
                    else if (err.code == 400 && userMessage == "Invalid start time.") then
                        pure unit
                    else pure unit
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Exception DETAILS start end (err.code) (codeMessage) url "" "")) *> (errorHandler err)

withAPIResultBT' url enableCache key f errorHandler flow = do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResultBT' url" url
    case resp of
        Right res -> if enableCache then do
                        _ <- pure $ setKeyInSharedPrefKeys key (toString (encode res))
                        (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
                        else (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
        Left  (err) -> do
            _ <- pure $ toggleBtnLoader "" false
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err
            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage

            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                deleteValueFromLocalStore REGISTERATION_TOKEN
                deleteValueFromLocalStore LANGUAGE_KEY
                deleteValueFromLocalStore REGISTRATION_APPROVED
                pure $ factoryResetApp ""
                  else pure unit
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end (err.code) (codeMessage) url "" "")) *> (errorHandler err)

---------------------------------------------------------------TriggerOTPBT Function---------------------------------------------------------------------------------------------------
triggerOTPBT :: TriggerOTPReq → FlowBT String TriggerOTPResp
triggerOTPBT payload = do
    _ <- lift $ lift $ doAff Readers.initiateSMSRetriever
    headers <- getHeaders' "" false
    withAPIResultBT (EP.triggerOTP "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if (errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
            pure $ toast (getString LIMIT_EXCEEDED)
            else pure $ toast (getString ERROR_OCCURED)
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguage -> chooseLanguage { props {btnActive = false} })
        BackT $ pure GoBack


makeTriggerOTPReq :: String -> TriggerOTPReq
makeTriggerOTPReq mobileNumber =
    let merchant = SC.getMerchantId ""
    in TriggerOTPReq
    {
      "mobileNumber"      : mobileNumber,
      "mobileCountryCode" : "+91",
      "merchantId" : if merchant == "NA" then getValueToLocalNativeStore MERCHANT_ID else merchant
    }

---------------------------------------------------------------TriggerSignatureOTPBT Function---------------------------------------------------------------------------------------------------

triggerSignatureBasedOTP :: SignatureAuthData → Flow GlobalState (Either ErrorResponse TriggerSignatureOTPResp)
triggerSignatureBasedOTP (SignatureAuthData signatureAuthData) = do
    Headers headers <- getHeaders "" false
    withAPIResult (EP.triggerSignatureOTP "") unwrapResponse $ callAPI (Headers (headers <> [Header "x-sdk-authorization" signatureAuthData.signature])) (TriggerSignatureOTPReq signatureAuthData.authData)
    where
        unwrapResponse (x) = x

----------------------------------------------------------- ResendOTPBT Function ------------------------------------------------------------------------------------------------------

resendOTPBT :: String -> FlowBT String ResendOTPResp
resendOTPBT token = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.resendOTP token) (\x → x) errorHandler (lift $ lift $ callAPI headers (ResendOTPRequest token))
    where
    errorHandler  errorPayload  = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if ( errorPayload.code == 400 && codeMessage == "AUTH_BLOCKED") then
            pure $ toast (getString LIMIT_EXCEEDED)
            else pure $ toast (getString ERROR_OCCURED)
        BackT $ pure GoBack



-------------------------------------------------------------VerifyTokenBT Function----------------------------------------------------------------------------------------------------
verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyTokenBT payload token = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.verifyToken token) (\x → x) errorHandler (lift $ lift $ callAPI headers (VerifyTokenRequest token payload))
    where
    errorHandler errorPayload = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if ( errorPayload.code == 400 && codeMessage == "TOKEN_EXPIRED") then
            pure $ toast (getString REQUEST_TIMED_OUT)
            else if ( errorPayload.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true}})
                void $ lift $ lift $ EHU.toggleLoader false
                pure $ toast "INVALID_AUTH_DATA"
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
                pure $ toast (getString LIMIT_EXCEEDED)
            else pure $ toast (getString ERROR_OCCURED)
        BackT $ pure GoBack

-- verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyToken payload token = do
    headers <- getHeaders "" false
    withAPIResult (EP.verifyToken token) unwrapResponse $  callAPI headers (VerifyTokenRequest token payload)
    where
        unwrapResponse (x) = x

makeVerifyOTPReq :: String -> VerifyTokenReq
makeVerifyOTPReq otp = VerifyTokenReq {
      "otp": otp,
      "deviceToken": if getValueToLocalNativeStore FCM_TOKEN == "__failed" then "generated_xxxx_xxxx_xxxx" else (getValueToLocalNativeStore FCM_TOKEN),
      "whatsappNotificationEnroll": OPT_IN
    }

-------------------------------------------------------------Logout BT Funtion---------------------------------------------------------------------------------------------------------

logOutBT :: LogOutReq -> FlowBT String LogOutRes
logOutBT payload = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.logout "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ SearchLocationBT Function ------------------------------------------------------------------------------------

searchLocationBT :: SearchLocationReq -> FlowBT String SearchLocationResp
searchLocationBT payload = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.autoComplete "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
  where
  errorHandler errorPayload  = do
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage  = SearchLocationModel}})
                BackT $ pure GoBack


makeSearchLocationReq :: String -> Number -> Number -> Int -> String -> String-> SearchLocationReq
makeSearchLocationReq input lat lng radius language components = SearchLocationReq {
    "input" : input,
    "location" : (show lat <> "," <> show lng),
    "radius" : radius,
    "components" : components,
    "language" : language,
    "sessionToken" : Nothing,
    "strictbounds": Nothing,
    "origin" : LatLong {
            "lat" : lat,
            "lon" : lng
            }
    }

------------------------------------------------------------------------ OnCallBT Function ------------------------------------------------------------------------------------

onCallBT :: OnCallReq -> FlowBT String OnCallRes
onCallBT payload = do
  headers <- getHeaders' "" false
  withAPIResultBT (EP.onCall "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
  where
    errorHandler errorPayload = BackT $ pure GoBack

makeOnCallReq :: String -> String -> OnCallReq
makeOnCallReq rideID callType = OnCallReq {
    "rideId" : rideID,
    "callType" : callType
}

------------------------------------------------------------------------ PlaceDetailsBT Function --------------------------------------------------------------------------------------
placeDetailsBT :: PlaceDetailsReq -> FlowBT String PlaceDetailsResp
placeDetailsBT (PlaceDetailsReq id) = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.placeDetails id) (\x → x) errorHandler (lift $ lift $ callAPI headers (PlaceDetailsReq id))
    where
    errorHandler errorPayload  = do
        pure $ toast (getString ERROR_OCCURED)
        _ <- lift $ lift $ EHU.toggleLoader false
        BackT $ pure GoBack

-- ------------------------------------------------------------------------ GetCoordinatesBT Function --------------------------------------------------------------------------------------
-- getCoordinatesBT :: GetCoordinatesReq -> FlowBT String GetCoordinatesResp
-- getCoordinatesBT (GetCoordinatesReq id language) = do
--     headers <- lift $ lift $ getHeaders ""
--     withAPIResultBT (EP.getCoordinates id language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetCoordinatesReq id language))
--     where
--     errorHandler errorPayload  = BackT $ pure GoBack


------------------------------------------------------------------------ RideSearchBT Function ----------------------------------------------------------------------------------------
rideSearchBT :: SearchReq -> FlowBT String SearchRes
rideSearchBT payload = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.searchReq "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            if errorPayload.code == 400 then
                pure $ toast (getString RIDE_NOT_SERVICEABLE)
              else pure $ toast (getString ERROR_OCCURED)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{currentStage = SearchLocationModel}})
            _ <- pure $ setValueToLocalStore LOCAL_STAGE "SearchLocationModel"
            BackT $ pure GoBack


makeRideSearchReq :: Number -> Number -> Number -> Number -> Address -> Address -> SearchReq
makeRideSearchReq slat slong dlat dlong srcAdd desAdd=
     SearchReq { "contents" : OneWaySearchReq{
                                               "destination" : SearchReqLocation {
                                                        "gps" : LatLong {
                                                            "lat" : dlat ,
                                                            "lon" : dlong
                                                            },
                                                        "address" : (LocationAddress desAdd)
                                               },
                                               "origin" : SearchReqLocation {
                                                "gps" : LatLong {
                                                            "lat" : slat ,
                                                            "lon" : slong
                                                },"address" : (LocationAddress srcAdd)
                                               }
                                              },
                 "fareProductType" : "ONE_WAY"
                }


------------------------------------------------------------------------ GetQuotes Function -------------------------------------------------------------------------------------------
getQuotes searchId = do
        headers <- getHeaders "" true
        withAPIResult (EP.getQuotes searchId) unwrapResponse $ callAPI headers (GetQuotesReq searchId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideConfirm Function ---------------------------------------------------------------------------------------
rideConfirm quoteId = do
        headers <- getHeaders "" false
        withAPIResult (EP.confirmRide quoteId) unwrapResponse $ callAPI headers (ConfirmRequest quoteId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ SelectEstimateBT Function ------------------------------------------------------------------------------------

selectEstimateBT :: DEstimateSelect -> String -> FlowBT String SelectEstimateRes
selectEstimateBT payload estimateId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.selectEstimate estimateId) (\x → x) errorHandler (lift $ lift $ callAPI headers (SelectEstimateReq estimateId payload))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

selectEstimate payload estimateId = do
        headers <- getHeaders "" false
        withAPIResult (EP.selectEstimate estimateId) unwrapResponse $ callAPI headers (SelectEstimateReq estimateId payload)
    where
        unwrapResponse (x) = x

makeEstimateSelectReq :: Boolean -> Maybe Int -> DEstimateSelect
makeEstimateSelectReq isAutoAssigned tipForDriver= DEstimateSelect {
      "customerExtraFee": tipForDriver,
      "autoAssignEnabled": isAutoAssigned,
      "autoAssignEnabledV2": isAutoAssigned
    }

------------------------------------------------------------------------ SelectList Function ------------------------------------------------------------------------------------------
selectList estimateId = do
        headers <- getHeaders "" true
        withAPIResult (EP.selectList estimateId) unwrapResponse $ callAPI headers (SelectListReq estimateId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideBooking Function -----------------------------------------------------------------------------------------
rideBooking bookingId = do
        headers <- getHeaders "" true
        withAPIResult (EP.ridebooking bookingId) unwrapResponse $ callAPI headers (RideBookingReq bookingId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ CancelRideBT Function ----------------------------------------------------------------------------------------
cancelRideBT :: CancelReq -> String -> FlowBT String CancelRes
cancelRideBT payload bookingId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.cancelRide bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CancelRequest payload bookingId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeCancelRequest :: HomeScreenState -> CancelReq
makeCancelRequest state = CancelReq {
    "additionalInfo" : Just state.props.cancelDescription
  , "reasonCode" : state.props.cancelReasonCode
  , "reasonStage" : "OnAssign"
  }

------------------------------------------------------------------------ CallDriver Function ------------------------------------------------------------------------------------------

callDriverBT :: String -> FlowBT String CallDriverRes
callDriverBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.callCustomerToDriver rideId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CallDriverReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ Feedback Function --------------------------------------------------------------------------------------------

rideFeedbackBT :: FeedbackReq -> FlowBT String FeedbackRes
rideFeedbackBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.feedback "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeFeedBackReq :: Int -> String -> String -> FeedbackReq
makeFeedBackReq rating rideId feedback = FeedbackReq
    {   "rating" : rating
    ,   "rideId" : rideId
    ,   "feedbackDetails" : feedback
    }


----------------------------------------------------------------------- RideBooking BT Function ---------------------------------------------------------------------------------------
rideBookingBT :: String -> FlowBT String RideBookingRes
rideBookingBT bookingId = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.ridebooking bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers  (RideBookingReq bookingId))
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

rideBookingList limit offset onlyActive = do
        headers <- getHeaders "" true
        withAPIResult (EP.rideBookingList limit offset onlyActive)  unwrapResponse $ callAPI headers (RideBookingListReq limit offset onlyActive)
    where
        unwrapResponse (x) = x


getProfileBT :: String -> FlowBT String GetProfileRes
getProfileBT _  = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.profile "") (\x → x) errorHandler (lift $ lift $ callAPI headers (GetProfileReq))
    where
    errorHandler (errorPayload) =  do
        BackT $ pure GoBack

-- updateProfileBT :: UpdateProfileReq -> FlowBT String UpdateProfileRes
updateProfile (UpdateProfileReq payload) = do
        headers <- getHeaders "" false
        withAPIResult (EP.profile "") unwrapResponse $ callAPI headers (UpdateProfileReq payload)
    where
        unwrapResponse (x) = x

mkUpdateProfileRequest :: LazyCheck -> UpdateProfileReq
mkUpdateProfileRequest _ =
    UpdateProfileReq{
          middleName : Nothing
        , lastName : Nothing
        , deviceToken : Just (getValueToLocalNativeStore FCM_TOKEN)
        , firstName : Nothing
        , email : Nothing
        , referralCode : Nothing
        , gender : Nothing
        , language : Just case getValueToLocalNativeStore LANGUAGE_KEY of
            "EN_US" -> "ENGLISH"
            "KN_IN" -> "KANNADA"
            "HI_IN" -> "HINDI"
            "ML_IN" -> "MALAYALAM"
            "BN_IN" -> "BENGALI"
            "TA_IN" -> "TAMIL"
            _       -> "ENGLISH"
        , clientVersion : Nothing
        , bundleVersion : Nothing
    }

editProfileRequest :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> UpdateProfileReq
editProfileRequest firstName middleName lastName emailID gender =
    UpdateProfileReq{
          middleName : middleName
        , lastName : lastName
        , deviceToken : Just (getValueToLocalNativeStore FCM_TOKEN)
        , firstName : firstName
        , email : emailID
        , referralCode : Nothing
        , gender : gender
        , language : Just case getValueToLocalNativeStore LANGUAGE_KEY of
            "EN_US" -> "ENGLISH"
            "KN_IN" -> "KANNADA"
            "HI_IN" -> "HINDI"
            "ML_IN" -> "MALAYALAM"
            "BN_IN" -> "BENGALI"
            "TA_IN" -> "TAMIL"
            _       -> "ENGLISH"
        , clientVersion : Nothing
        , bundleVersion : Nothing
    }

placeNameBT :: GetPlaceNameReq -> FlowBT String GetPlaceNameResp
placeNameBT payload = do
     headers <- lift $ lift $ getHeaders "" true
     withAPIResultBT (EP.getPlaceName "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = BackT $ pure GoBack

makePlaceNameReq :: Number -> Number -> String -> GetPlaceNameReq
makePlaceNameReq lat lng language = GetPlaceNameReq
    {"sessionToken" : Just "",
      "language" : Just language,
      "getBy" : GetPlaceNameBy {
          "tag" : "ByLatLong",
          "contents" :LatLongType ( LatLonBody {
              "lat" : lat,
              "lon" : lng
          })
      }
    }

makePlaceNameReqByPlaceId :: String -> String -> GetPlaceNameReq
makePlaceNameReqByPlaceId placeId language = GetPlaceNameReq
    {"sessionToken" : Just "",
      "language" : Just language,
      "getBy" : GetPlaceNameBy {
          "tag" : "ByPlaceId",
          "contents" : (PlaceId placeId)
      }
    }

getRouteBT :: String -> GetRouteReq -> FlowBT String GetRouteResp
getRouteBT routeState body = do
     headers <- lift $ lift $ getHeaders "" true
     withAPIResultBT (EP.getRoute routeState) (\x → x) errorHandler (lift $ lift $ callAPI headers (RouteReq routeState body))
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeGetRouteReq :: Number -> Number -> Number -> Number -> GetRouteReq
makeGetRouteReq slat slng dlat dlng = GetRouteReq {
    "waypoints": [
      LatLong {
          "lon": slng,
          "lat": slat
      },
      LatLong{
          "lon": dlng,
          "lat": dlat
      }],
    "mode": Just "CAR",
    "calcPoints": true
}

walkCoordinate :: Number -> Number -> Number -> Number -> Locations
walkCoordinate srcLat srcLon destLat destLong = {
    "points": [
      {
        "lat": srcLat,
        "lng": srcLon
      },
      {
        "lat": destLat,
        "lng": destLong
      }
    ]
}

walkCoordinates :: Snapped -> Locations
walkCoordinates (Snapped points) =
  { "points": map (\(LatLong item) -> { "lat": item.lat, "lng": item.lon }) points
  }

postContactsReq :: (Array NewContacts) -> EmergContactsReq
postContactsReq contacts = EmergContactsReq {
  "defaultEmergencyNumbers" : map (\item -> ContactDetails {
      "mobileNumber": item.number,
      "name": item.name,
      "mobileCountryCode": "+91"
  }) contacts
}

emergencyContactsBT :: EmergContactsReq -> FlowBT String EmergContactsResp
emergencyContactsBT req = do
    headers <- lift $ lift $ getHeaders "" false
    withAPIResultBT (EP.emergencyContacts "") (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getEmergencyContactsBT ::  GetEmergContactsReq -> FlowBT String GetEmergContactsResp
getEmergencyContactsBT req = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.emergencyContacts "") (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getDriverLocationBT :: String -> FlowBT String GetDriverLocationResp
getDriverLocationBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getCurrentLocation rideId) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetDriverLocationReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

getDriverLocation rideId = do
        headers <- getHeaders "" false
        withAPIResult (EP.getCurrentLocation rideId) unwrapResponse $ callAPI headers (GetDriverLocationReq rideId)
    where
        unwrapResponse (x) = x

getRoute routeState payload = do
    headers <- getHeaders "" true
    withAPIResult (EP.getRoute routeState) unwrapResponse $  callAPI headers (RouteReq routeState payload)
    where
        unwrapResponse (x) = x

addSavedLocationBT :: SavedReqLocationAPIEntity -> FlowBT String AddLocationResp
addSavedLocationBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.addLocation "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

getSavedLocationBT :: SavedLocationReq -> FlowBT String SavedLocationsListRes
getSavedLocationBT payload = do
    headers <- getHeaders' "" true
    withAPIResultBT (EP.savedLocation "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

getSavedLocationList payload = do
        headers <- getHeaders "" true
        withAPIResult (EP.savedLocation "") unwrapResponse $ callAPI headers (SavedLocationReq)
    where
        unwrapResponse (x) = x

deleteSavedLocationBT :: DeleteSavedLocationReq -> FlowBT String DeleteSavedLocationRes
deleteSavedLocationBT (DeleteSavedLocationReq tag) = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.deleteLocation tag) (\x -> x) errorHandler (lift $ lift $ callAPI headers (DeleteSavedLocationReq tag))
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

sendIssueBT :: SendIssueReq -> FlowBT String SendIssueRes
sendIssueBT req = do
    headers <- getHeaders' "" false
    withAPIResultBT ((EP.sendIssue "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

----------------------------------------------------------------------------------------------
drawMapRoute :: Number -> Number -> Number -> Number -> Markers -> String -> String -> String -> Maybe Route -> String -> SpecialLocationTag -> FlowBT String (Maybe Route)
drawMapRoute srcLat srcLng destLat destLng markers routeType srcAddress destAddress existingRoute routeAPIType specialLocation = do
    _ <- pure $ removeAllPolylines ""
    case existingRoute of
        Just (Route route) -> do
            let (Snapped points) = route.points
            case points of
                [] -> do
                    (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng)
                    callDrawRoute ((routeResponse) !! 0)
                _  -> callDrawRoute existingRoute
        Nothing -> do
            (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng)
            _ <- pure $ printLog "drawRouteResponse" routeResponse
            let ios = (os == "IOS")
            let route = ((routeResponse) !! 0)
            callDrawRoute route
    where
        callDrawRoute :: Maybe Route -> FlowBT String (Maybe Route)
        callDrawRoute route =
            case route of
                Just (Route routes) ->
                    if (routes.distance <= 50000) then do
                      lift $ lift $ liftFlow $ drawRoute (walkCoordinates routes.points) "LineString" "#323643" true markers.srcMarker markers.destMarker 8 routeType srcAddress destAddress specialLocation
                      pure route
                    else do
                      lift $ lift $ liftFlow $ drawRoute (walkCoordinate srcLat srcLng destLat destLng) "DOT" "#323643" false markers.srcMarker markers.destMarker 8 routeType srcAddress destAddress specialLocation
                      pure route
                Nothing -> pure route

type Markers = {
    srcMarker :: String,
    destMarker :: String
}

driverTracking :: String -> Markers
driverTracking _ = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ic_auto_map" else if (getMerchant FunctionCall == YATRISATHI && os == "IOS") then "ic_vehicle_nav_on_map" else "ny_ic_vehicle_nav_on_map",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "src_marker" else "ny_ic_src_marker"
}

rideTracking :: String -> Markers
rideTracking _ = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ic_auto_map" else if (getMerchant FunctionCall == YATRISATHI && os == "IOS") then "ic_vehicle_nav_on_map" else "ny_ic_vehicle_nav_on_map",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "dest_marker" else "ny_ic_dest_marker"
}

normalRoute ::String -> Markers
normalRoute _ = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "src_marker" else "ny_ic_src_marker",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "dest_marker" else "ny_ic_dest_marker"
}


makeSendIssueReq :: Maybe String ->  Maybe String -> String -> String -> SendIssueReq
makeSendIssueReq email bookingId reason description= SendIssueReq {
    "contactEmail" : email ,
    "rideBookingId" : bookingId ,
    "issue" : Issue {
            "reason" : reason,
            "description" : description
        }
}

originServiceabilityBT :: ServiceabilityReq -> FlowBT String ServiceabilityRes
originServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityOrigin "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

destServiceabilityBT :: DestinationServiceabilityReq -> FlowBT String ServiceabilityResDestination
destServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityDest "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

makeServiceabilityReq :: Number -> Number -> ServiceabilityReq
makeServiceabilityReq latitude longitude = ServiceabilityReq {
    "location" : LatLong {
                "lat" : latitude,
                "lon" : longitude
            }
    }

makeServiceabilityReqForDest :: Number -> Number -> DestinationServiceabilityReq
makeServiceabilityReqForDest latitude longitude = DestinationServiceabilityReq {
    "location" : LatLong {
                "lat" : latitude,
                "lon" : longitude
            }
    }

---------------------------------------------------------------- flowStatus function -------------------------------------------------------------------
flowStatusBT :: String -> FlowBT String FlowStatusRes
flowStatusBT dummy = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.flowStatus "") (\x → x) errorHandler (lift $ lift $ callAPI headers FlowStatusReq)
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

---------------------------------------------------------------- notifyFlowEvent function -------------------------------------------------------------------

notifyFlowEvent requestBody = do
    headers <- getHeaders "" false
    withAPIResult (EP.notifyFlowEvent "") unwrapResponse $ callAPI headers requestBody
    where
        unwrapResponse (x) = x

notifyFlowEventBT :: NotifyFlowEventReq -> FlowBT String NotifyFlowEventRes
notifyFlowEventBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.notifyFlowEvent "") (\x → x) errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeNotifyFlowEventReq :: String -> NotifyFlowEventReq
makeNotifyFlowEventReq event = NotifyFlowEventReq { "event" : event }

------------------------------------------------------------------------ CancelEstimate Function ------------------------------------------------------------------------------------

cancelEstimate estimateId = do
    headers <- getHeaders "" false
    withAPIResult (EP.cancelEstimate estimateId) unwrapResponse $ callAPI headers (CancelEstimateReq estimateId)
    where
        unwrapResponse (x) = x

cancelEstimateBT :: String -> FlowBT String CancelEstimateRes
cancelEstimateBT estimateId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.cancelEstimate estimateId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CancelEstimateReq estimateId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

userSosBT :: UserSosReq -> FlowBT String UserSosRes
userSosBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSos "") (\x → x) errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

userSosStatusBT :: String ->  SosStatus -> FlowBT String UserSosStatusRes
userSosStatusBT sosId requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSosStatus sosId) (\x → x) errorHandler (lift $ lift $ callAPI headers (UserSosStatusReq sosId requestBody))
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeUserSosReq :: UserSosFlow -> String -> UserSosReq
makeUserSosReq flow rideId = UserSosReq {
     "flow" : flow,
     "rideId" : rideId
}

createUserSosFlow :: String -> String -> UserSosFlow
createUserSosFlow tag content = UserSosFlow {
    "tag" : tag,
    "contents" : content
}

makeSosStatus :: String -> SosStatus
makeSosStatus sosStatus = SosStatus {
     "status" : sosStatus
}

