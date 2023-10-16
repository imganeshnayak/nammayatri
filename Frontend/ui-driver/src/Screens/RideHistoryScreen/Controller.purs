{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideHistoryScreen.Controller where

import Effect.Unsafe
import Prelude
import Screens.Types (RideHistoryScreenState, AnimationState(..), ItemState(..), IndividualRideCardState(..), DisabilityType(..))
import Log
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.DatePickerModel as DatePickerModel
import Components.ErrorModal as ErrorModalController
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PaymentHistoryListItem as PaymentHistoryModelItem
import Components.PaymentHistoryModel as PaymentHistoryModel
import Components.PrimaryButton as PrimaryButton
import Data.Array (union, (!!), filter, length)
import Data.Int (ceil)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (getNewIDWithTag, strToBool)
import Engineering.Helpers.LogEvent (logEvent)
import JBridge (cleverTapCustomEvent, metaLogEvent, firebaseLogEvent)
import Helpers.Utils (setRefreshing, setEnabled, parseFloat, getRideLabelData, convertUTCtoISC, getRequiredTag, incrementValueOfLocalStoreKey)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import PrestoDOM (Eval, continue, exit, ScrollState(..), updateAndExit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress, tripDatesCount)
import Screens (ScreenName(..), getScreen)
import Services.API (RidesInfo(..), Status(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore)
import Styles.Colors as Color

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen RIDE_HISTORY_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen RIDE_HISTORY_SCREEN)
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    OnFadeComplete str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "on_fade_complete"
    Refresh -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "refresh"
    SelectTab str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "select_tab"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    IndividualRideCardAction (IndividualRideCardController.Select index)-> do
      trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "individual_ride_card_action" "select"
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    Loader -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "load_more"
    Scroll str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "scroll_event"
    ScrollStateChanged scrollState -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "scroll_state_changed"
    RideHistoryAPIResponseAction resp -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "ride_history_response_action"
    ErrorModalActionController action -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "error_modal_action"
    Dummy -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "dummy_action"
    NoAction -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "no_action"
    ShowDatePicker -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "show_date_picker_action"
    DatePickerAC act -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "date_picker_model" "on_date_select"
    GenericHeaderAC act -> case act of
      GenericHeader.PrefixImgOnClick -> do
          trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "generic_header" "back_icon_on_click"
          trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
      GenericHeader.SuffixImgOnClick -> do
          trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "generic_header" "forward_icon_on_click"
          trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
    PaymentHistoryModelAC act -> pure unit
    OpenPaymentHistory -> pure unit

data ScreenOutput = GoBack
                    | HomeScreen
                    | GoToFilter String
                    | ProfileScreen
                    | GoToTripDetails RideHistoryScreenState
                    | RefreshScreen RideHistoryScreenState
                    | LoaderOutput RideHistoryScreenState
                    | GoToNotification
                    | GoToReferralScreen
                    | SelectedTab RideHistoryScreenState
                    | OpenPaymentHistoryScreen RideHistoryScreenState
                    | SubscriptionScreen RideHistoryScreenState

data Action = Dummy
            | OnFadeComplete String
            | Refresh
            | BackPressed
            | SelectTab String
            | BottomNavBarAction BottomNavBar.Action
            | IndividualRideCardAction IndividualRideCardController.Action
            | RideHistoryAPIResponseAction (Array RidesInfo)
            | Loader
            | Scroll String
            | ErrorModalActionController ErrorModalController.Action
            | NoAction
            | AfterRender
            | ScrollStateChanged ScrollState
            | DatePickerAC DatePickerModel.Action
            | ShowDatePicker
            | GenericHeaderAC GenericHeader.Action
            | PaymentHistoryModelAC PaymentHistoryModel.Action
            | OpenPaymentHistory

eval :: Action -> RideHistoryScreenState -> Eval Action ScreenOutput RideHistoryScreenState
eval AfterRender state = continue state
eval BackPressed state = if state.props.showPaymentHistory then continue state{ props {showPaymentHistory = false }}
                          else exit GoBack
eval (OnFadeComplete _ ) state = if (not state.recievedResponse) then continue state else
  continue state { shimmerLoader = case state.shimmerLoader of
                              AnimatedIn ->AnimatedOut
                              AnimatingOut -> AnimatedOut
                              a -> a  }

eval Refresh state = do
  exit $ RefreshScreen state

eval (ScrollStateChanged scrollState) state = do
  _ <- case scrollState of
           SCROLL_STATE_FLING ->
               pure $ setEnabled "2000030" false
           _ ->
               pure unit
  continue state

eval (SelectTab tab) state = updateAndExit state $ SelectedTab state{currentTab = tab, datePickerState { activeIndex = tripDatesCount - 1 , selectedItem {date = 0, month = "", year = 0}}} 

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen
    "Profile" -> exit $ ProfileScreen
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.logField "ny_driver_alert_click"
      exit $ GoToNotification
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    "Join" -> do
      let driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_SUBSCRIPTION
      _ <- pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      _ <- pure $ metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let _ = unsafePerformEffect $ firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (IndividualRideCardAction (IndividualRideCardController.Select index)) state = do
  let filteredRideList = rideListFilter state.currentTab state.rideList
  exit $ GoToTripDetails state {
      selectedItem = (fromMaybe dummyCard (filteredRideList !! index))
  }
eval Loader state = do
  exit $ LoaderOutput state{loaderButtonVisibility = false}

eval (RideHistoryAPIResponseAction rideList) state = do
  let bufferCardDataPrestoList = (rideHistoryListTransformer rideList)
  let filteredRideList = (rideListResponseTransformer rideList)
  _ <- pure $ setRefreshing "2000030" false
  let loadBtnDisabled = if(length rideList == 0) then true else false
  continue $ state {shimmerLoader = AnimatedOut, recievedResponse = true,rideList = union(state.rideList) (filteredRideList) ,prestoListArrayItems =  union (state.prestoListArrayItems) (bufferCardDataPrestoList), loadMoreDisabled = loadBtnDisabled}

eval (Scroll value) state = do
  -- TODO : LOAD MORE FUNCTIONALITY
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let canScrollUp = fromMaybe true (strToBool (fromMaybe "true" ((split (Pattern ",")(value))!!3)))
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  _ <- if canScrollUp then (pure $ setEnabled "2000030" false) else  (pure $ setEnabled "2000030" true)
  continue state { loaderButtonVisibility = loadMoreButton}

eval (DatePickerAC (DatePickerModel.OnDateSelect idx item)) state = do
  let newState = state{datePickerState{activeIndex = idx, selectedItem = item},rideList = [], prestoListArrayItems = []}
  exit $ SelectedTab newState

eval ShowDatePicker state = continue state{props{showDatePicker = not state.props.showDatePicker}}

eval OpenPaymentHistory state = exit $ OpenPaymentHistoryScreen state

eval (PaymentHistoryModelAC (PaymentHistoryModel.GenericHeaderAC (GenericHeader.PrefixImgOnClick))) state = continue state{props{showPaymentHistory = false}}

eval (PaymentHistoryModelAC (PaymentHistoryModel.ErrorModalActionController (ErrorModalController.PrimaryButtonActionController PrimaryButton.OnClick))) state = continue state{props{showPaymentHistory = false}}

eval (PaymentHistoryModelAC (PaymentHistoryModel.PaymentHistoryListItemAC (PaymentHistoryModelItem.OnClick id))) state = do
  let updatedData = map (\item -> if item.id == id then item{isSelected = not item.isSelected} else item) state.data.paymentHistory.paymentHistoryList
  continue state{data{paymentHistory { paymentHistoryList = updatedData}}}

eval _ state = continue state

rideHistoryListTransformer :: Array RidesInfo -> Array ItemState
rideHistoryListTransformer list = (map (\(RidesInfo ride) ->
  let accessibilityTag = (getDisabilityType ride.disabilityTag)
    in 
      {
      date : toPropValue (convertUTCtoISC (ride.createdAt) "D MMM"),
      time : toPropValue (convertUTCtoISC (ride.createdAt )"h:mm A"),
      total_amount : toPropValue $ fromMaybe ride.estimatedBaseFare ride.computedFare,
      card_visibility : toPropValue "visible",
      shimmer_visibility : toPropValue "gone",
      rideDistance : toPropValue $ (parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2) <> " km Ride" <> case ride.riderName of 
                              Just name -> " with " <> name
                              Nothing -> "",
      status :  toPropValue ride.status,
      vehicleModel : toPropValue ride.vehicleModel ,
      shortRideId : toPropValue ride.shortRideId  ,
      vehicleNumber :  toPropValue ride.vehicleNumber  ,
      driverName : toPropValue ride.driverName  ,
      driverSelectedFare : toPropValue ride.driverSelectedFare  ,
      vehicleColor : toPropValue ride.vehicleColor  ,
      id : toPropValue ride.shortRideId,
      updatedAt : toPropValue ride.updatedAt,
      source : toPropValue (decodeAddress (ride.fromLocation) false),
      destination : toPropValue (decodeAddress (ride.toLocation) false),
      amountColor: toPropValue (case (ride.status) of
                    "COMPLETED" -> Color.black800
                    "CANCELLED" -> Color.red
                    _ -> Color.black800),
      riderName : toPropValue $ fromMaybe "" ride.riderName,
      metroTagVisibility : toPropValue if (ride.specialLocationTag /= Nothing && (getRequiredTag "text" ride.specialLocationTag accessibilityTag) /= Nothing && (isNothing accessibilityTag)) then "visible" else "gone",
      accessibilityTagVisibility : toPropValue if (isJust accessibilityTag) then "visible" else "gone",
      specialZoneText : toPropValue $ getRideLabelData "text" (if (isJust accessibilityTag) then Just "Purple_Ride" else ride.specialLocationTag) Nothing,
      specialZoneImage : toPropValue $ getRideLabelData "imageUrl" ride.specialLocationTag accessibilityTag,
      specialZoneLayoutBackground : toPropValue $ getRideLabelData "backgroundColor" ride.specialLocationTag accessibilityTag

    }) list )

getDisabilityType :: Maybe String -> Maybe DisabilityType
getDisabilityType disabilityString = case disabilityString of 
                                      Just "BLIND_LOW_VISION" -> Just BLIND_AND_LOW_VISION
                                      Just "HEAR_IMPAIRMENT" -> Just HEAR_IMPAIRMENT
                                      Just "LOCOMOTOR_DISABILITY" -> Just LOCOMOTOR_DISABILITY
                                      Just "OTHER" -> Just OTHER_DISABILITY
                                      _ -> Nothing

rideListResponseTransformer :: Array RidesInfo -> Array IndividualRideCardState
rideListResponseTransformer list = (map (\(RidesInfo ride) -> {
    date : (convertUTCtoISC (ride.createdAt) "D MMM"),
    time : (convertUTCtoISC (ride.createdAt )"h:mm A"),
    total_amount : (case (ride.status) of
                    "CANCELLED" -> 0
                    _ -> fromMaybe ride.estimatedBaseFare ride.computedFare),
    card_visibility : (case (ride.status) of
                        "CANCELLED" -> "gone"
                        _ -> "visible"),
    shimmer_visibility : "gone",
    rideDistance :  parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2,
    status :  (ride.status),
    vehicleModel : ride.vehicleModel ,
    shortRideId : ride.shortRideId  ,
    vehicleNumber :  ride.vehicleNumber  ,
    driverName : ride.driverName  ,
    driverSelectedFare : ride.driverSelectedFare  ,
    vehicleColor : ride.vehicleColor  ,
    id : ride.shortRideId,
    updatedAt : ride.updatedAt,
    source : (decodeAddress (ride.fromLocation) false),
    destination : (decodeAddress (ride.toLocation) false),
    vehicleType : ride.vehicleVariant,
    riderName : fromMaybe "" ride.riderName
}) list )


prestoListFilter :: String -> Array ItemState -> Array ItemState
prestoListFilter statusType list = (filter (\(ride) -> (ride.status == (toPropValue statusType)) ) list )

rideListFilter :: String -> Array IndividualRideCardState -> Array IndividualRideCardState
rideListFilter statusType list = (filter (\(ride) -> (ride.status == statusType) ) list )

dummyCard :: IndividualRideCardState
dummyCard =  {
    date : "",
    time : "",
    total_amount : 0,
    card_visibility : "",
    shimmer_visibility : "",
    rideDistance : "",
    status : "",
    vehicleModel : "",
    shortRideId : "",
    vehicleNumber : "",
    driverName : "",
    driverSelectedFare : 0,
    vehicleColor : "",
    id : "",
    updatedAt : "",
    source : "",
    destination : "",
    vehicleType : "",
    riderName : ""
  }
