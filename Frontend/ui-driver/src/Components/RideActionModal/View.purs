{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RideActionModal.View where

import Common.Types.App

import Components.RideActionModal.Controller (Action(..), Config)
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (countDown, getSpecialZoneConfig,getRequiredTag,clearTimer,getCurrentUTC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, not, pure, show, unit, discard, ($), (/=), (<>), (&&), (==), (-), (>), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alpha, background, clickable, color, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, width, imageWithFallback, fontSize, weight)
import PrestoDOM.Properties (cornerRadii, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (KeyStore(..), getValueToLocalStore,setValueToLocalStore)
import Styles.Colors as Color 
import Engineering.Helpers.Commons (screenWidth)
import Screens.Types (HomeScreenStage(..),TimerStatus(..))
import JBridge (getVersionCode,waitingCountdownTimer,toast, showAndHideLoader)
import Merchant.Utils(getMerchant, Merchant(..))
import Types.App (defaultGlobalState)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , margin $ MarginBottom 16
      ][ messageButton push config
       , callButton push config
       , openGoogleMap push config 
      ]
    , if config.specialLocationTag == Maybe.Nothing || (getRequiredTag "text" config.specialLocationTag) == Maybe.Nothing
        then rideActionView push config else rideActionViewWithZone push config
    ]

messageButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
messageButton push config =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) && checkVersionForChat (getCurrentAndroidVersion (getMerchant unit)) then VISIBLE else GONE
  , padding $ Padding 20 16 20 16
  , margin $ MarginLeft 16
  , background Color.white900
  , stroke $ "1,"<> Color.black500
  , cornerRadius 30.0
  , onClick (\action -> do
                  if not config.isChatOpened then showAndHideLoader 5000.0 (getString LOADING) (getString PLEASE_WAIT) defaultGlobalState
                  else pure unit
                  push action
            ) (const MessageCustomer)
  ][  imageView
      [ imageWithFallback if config.unReadMessages then "ic_chat_badge,https://assets.juspay.in/nammayatri/images/driver/ic_chat_badge.png" else "ic_chat,https://assets.juspay.in/nammayatri/images/driver/ic_chat.png"
      , height $ V 20
      , width $ V 20
      ]
  ]

getCurrentAndroidVersion :: Merchant -> Int 
getCurrentAndroidVersion merchant = 
  case merchant of 
    NAMMAYATRIPARTNER -> 54
    YATRIPARTNER -> 47 
    JATRISAATHIDRIVER -> 1

checkVersionForChat :: Int -> Boolean
checkVersionForChat reqVersion =
  let currVersion = unsafePerformEffect getVersionCode
    in currVersion > reqVersion

callButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
callButton push config =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , padding $ Padding 20 16 20 16
  , margin $ MarginLeft 8
  , background Color.white900
  , stroke $ "1,"<> Color.black500
  , cornerRadius 30.0
  , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) then VISIBLE else GONE
  , onClick push (const $ CallCustomer)
  ][  imageView
      [ imageWithFallback "ic_phone,https://assets.juspay.in/nammayatri/images/common/ic_phone.png"
      , height $ V 20
      , width $ V 20
      ]
  ]
  
rideActionViewWithZone :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit) w
rideActionViewWithZone push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background $ getSpecialZoneConfig "backgroundColor" config.specialLocationTag
  , cornerRadii $ Corners 25.0 true true false false
  , orientation VERTICAL
  , padding $ PaddingTop 5
  , gravity CENTER
  , visibility if config.specialLocationTag == Maybe.Nothing then GONE else VISIBLE
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      ][ imageView
          [ width $ V 18
          , height $ V 18
          , imageWithFallback $ getSpecialZoneConfig "imageUrl" config.specialLocationTag
          ]
        , textView 
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , text $ getSpecialZoneConfig "text" config.specialLocationTag
          , gravity CENTER_VERTICAL
          , color Color.white900
          , margin $ MarginLeft 5
          , textSize FontSize.a_12
          , fontStyle $ FontStyle.medium TypoGraphy
          ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 25.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding $ PaddingTop 6
      , margin $ MarginTop 6
      , gravity CENTER
      , stroke $ "1," <> Color.grey800
      ][  rideActionDataView push config
        , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.lightGrey
          , margin $ MarginTop 24
          ][]
        , if config.startRideActive then startRide push config else endRide push config
        , cancelRide push config
      ]
  ]

rideActionView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionView push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadii $ Corners 25.0 true true false false
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingTop 6
  , gravity CENTER
  , stroke $ "1," <> Color.grey800
  ][  rideActionDataView push config
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.lightGrey
      , margin $ MarginTop 24
      ][]
    , if config.startRideActive then startRide push config else endRide push config
    , cancelRide push config
  ]


openGoogleMap :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
openGoogleMap push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity RIGHT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 24 16 24 16
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][  imageView
          [ width $ V 20
          , height $ V 20
          , imageWithFallback "ny_ic_navigation,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigation.png"
          ]
        , textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin (MarginLeft 8)
          , text (getString MAPS)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
      ]
  ] 

rideActionDataView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionDataView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , gravity CENTER
    ][  linearLayout
          [ width (V 34)
          , height (V 4)
          , cornerRadius 4.0
          , background Color.black500
          ][]
      , customerNameView push config
      , linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ][ rideInfoView push config
            , if config.startRideActive then sourceAndDestinationView push config else destinationView config push
            ]
          ]
      ]

totalDistanceView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
totalDistanceView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , orientation VERTICAL
    , weight 1.0
    ][ textView $ 
       [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString RIDE_DISTANCE)
        , color Color.black650
        , textSize FontSize.a_14
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.totalDistance
        , color Color.black650
        , textSize FontSize.a_20
        , ellipsize true
        , singleLine true
        , fontStyle (FontStyle.medium TypoGraphy)
        ]
    ]

sourceAndDestinationView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceAndDestinationView push config = 
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 24 24
    ][  sourceDestinationImageView config
      , sourceDestinationTextView push config
      ]

startRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
startRide push config = 
  linearLayout
  [ width MATCH_PARENT
  , height (V 50)
  , background Color.darkMint
  , cornerRadius 8.0
  , margin $ Margin 16 16 16 0
  , gravity CENTER
  , onClick push (const $ StartRide)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString START_RIDE)
      , color Color.white900
      , padding (Padding 0 0 0 4)
      ] <> FontStyle.subHeading1 TypoGraphy
      )
  ]

endRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
endRide push config = 
  linearLayout
  [ width MATCH_PARENT
  , height (V 50)
  , background Color.red
  , cornerRadius 8.0
  , margin $ Margin 16 16 16 24
  , gravity CENTER
  , onClick push (const $ EndRide)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString END_RIDE)
      , color Color.white900
      , padding (Padding 0 0 0 4)
      ] <> FontStyle.subHeading1 TypoGraphy
      )
  ]

cancelRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cancelRide push config = 
  linearLayout 
  [ width MATCH_PARENT
  , height (V 34)
  , gravity CENTER
  , visibility if config.startRideActive then VISIBLE else GONE
  , margin $ MarginVertical 16 16
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 16 8 16 8
      , text (getString CANCEL_RIDE)
      , color Color.red
      , onClick push (const CancelRide)
      ] <> FontStyle.body1 TypoGraphy
      )
  ]

customerNameView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
customerNameView push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL  
  , gravity CENTER_VERTICAL
  , margin $ MarginVertical 16 20
  ][  linearLayout
      [ height WRAP_CONTENT
      , width  WRAP_CONTENT
      , orientation VERTICAL
      , gravity START
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT 
          , text $ getTitle config
          , color Color.greyTextColor
          , ellipsize true
          , singleLine false
          ] <> FontStyle.subHeading2 TypoGraphy
        , arrivedButtonView push config
        ]
    ]

estimatedFareView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedFareView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , orientation VERTICAL
    , weight 1.0
    ][ textView $ 
       [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString RIDE_FARE)
        , color Color.black650
        , textSize FontSize.a_14
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text ("₹" <> (show config.estimatedRideFare))
        , color Color.black900
        , ellipsize true
        , textSize FontSize.a_20
        , singleLine true
        , fontStyle (FontStyle.semiBold TypoGraphy)
        ]
    ]

waitTimeView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
waitTimeView push config = 
   linearLayout
     [ height WRAP_CONTENT
     , gravity START
     , orientation VERTICAL
     , weight 1.0
     , visibility if (config.waitTime /= "__") && (config.notifiedCustomer) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "NoView" ) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "Stop" ) then VISIBLE else GONE
     ]
     [ linearLayout
         [
          orientation HORIZONTAL
         ]
         [textView $ 
        [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , text (getString WAIT_TIME)
         , color Color.black650
         , textSize FontSize.a_14
         , ellipsize true
         , singleLine true
         ] <> FontStyle.body1 TypoGraphy
        ,
        imageView
          [ height MATCH_PARENT
            , width  $ V 40
            , visibility if config.notifiedCustomer then VISIBLE else GONE
            , onClick push (const WaitingInfo)
            , imageWithFallback "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png"
          ]
         ]
        , textView $ 
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , text (config.waitTime)
         , color Color.black900
         , ellipsize true
         , textSize FontSize.a_20
         , singleLine true
         , fontStyle (FontStyle.semiBold TypoGraphy)
         ]
     ]


rideInfoView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideInfoView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke ("1," <> Color.grey900)
    , cornerRadius 8.0
    , padding (Padding 16 16 5 16) 
    ][ estimatedFareView push config
     , separator true
     , totalDistanceView push config
     , separator $ (config.waitTime /= "__") && (config.notifiedCustomer) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "NoView" ) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "Stop" )
     , waitTimeView push config
     , linearLayout
       [ weight 1.0
       , height MATCH_PARENT
       ][]
    ]

separator :: forall w . Boolean -> PrestoDOM (Effect Unit) w
separator visibility' = 
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , margin $ MarginHorizontal 5 5
    , visibility if visibility' then VISIBLE else GONE
    ][ linearLayout
      [ width $ V 1
      , background Color.lightGrey
      , height MATCH_PARENT
      ][]
    ]

sourceDestinationImageView :: forall w . Config -> PrestoDOM (Effect Unit) w
sourceDestinationImageView  config = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][ imageView
        [ height $ V 21
        , width $ V 17
        , margin $ MarginTop 2
        , imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
        ]
      , lineImageView 53
      , imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageWithFallback "ny_ic_destination,https://assets.juspay.in/nammayatri/images/driver/ny_ic_destination.png"         
        ]
      ]


sourceDestinationTextView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceDestinationTextView push config =
  linearLayout 
    [ width WRAP_CONTENT 
    , orientation VERTICAL 
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.sourceAddress.titleText
        , color Color.black800
        , ellipsize true
        , singleLine true
        , textSize FontSize.a_16 
        ] <> FontStyle.subHeading1 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.sourceAddress.detailText
        , color Color.black650
        , margin (MarginBottom 30)
        , ellipsize true
        , singleLine true
        , textSize FontSize.a_14 
        ] <> FontStyle.body1 TypoGraphy
      , destAddressTextView config push
      ]   

arrivedButtonView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
arrivedButtonView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , cornerRadius 24.0
  , gravity CENTER_VERTICAL
  , stroke $ if config.notifiedCustomer then "0," <> Color.blackLessTrans  else "1," <> Color.blue900 
  , background if config.notifiedCustomer then Color.grey700 else Color.white900
  , padding (Padding 10 7 12 7)
  , margin (MarginTop 12)
  , onClick (\action -> do 
      if config.notifiedCustomer then pure unit
        else do
          _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP (show Triggered)
          _ <- pure $ setValueToLocalStore SET_WAITING_TIME (getCurrentUTC "")
          _ <- waitingCountdownTimer 0 push TimerCallback
          _ <- countDown config.buttonTimeOut config.id push ButtonTimer
          push action) (const NotifyCustomer)
  , visibility if config.isDriverArrived then VISIBLE else GONE
  ][  imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback if config.notifiedCustomer then "ny_ic_tick_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_tick_grey.png" else "ic_chat_blue,https://assets.juspay.in/nammayatri/images/driver/ic_chat_blue.png"
      , margin $ MarginRight 4
      ]
    , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text if config.notifiedCustomer then (getString CUSTOMER_NOTIFIED) else (getString I_HAVE_ARRIVED)
      , color if config.notifiedCustomer then Color.black800 else Color.blue900
      , gravity CENTER
      ]<> FontStyle.body1 TypoGraphy
    ]

destinationView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationView config push = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 24 24
  ][  imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback "ny_ic_loc_red,https://assets.juspay.in/nammayatri/images/common/ny_ic_loc_red.png"
      , margin $ Margin 0 3 8 0         
      ]
    , destAddressTextView config push
  ]

lineImageView :: forall w . Int -> PrestoDOM (Effect Unit) w
lineImageView val = 
  imageView
    [ height $ V val
    , width $ V 15
    , imageUrl "ic_line"
    , margin $ MarginLeft 7
    ]

destAddressTextView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destAddressTextView config push= 
  linearLayout 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.destinationAddress.titleText
        , color Color.black800
        , ellipsize true
        , singleLine true
        , textSize FontSize.a_16 
        ] <> FontStyle.subHeading1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.destinationAddress.detailText
        , color Color.black650
        , ellipsize true
        , textSize FontSize.a_14
        , maxLines if config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer then 1 else 2
        ]<> FontStyle.body1 TypoGraphy
      ]

getTitle :: Config -> String
getTitle config = case config.startRideActive of
  false -> (getString YOU_ARE_ON_A_RIDE)
  true  -> case config.isDriverArrived, config.notifiedCustomer of 
    false, false  -> (config.customerName <> " " <> (getString IS_WAITING_FOR_YOU) <> "...") 
    true, _       -> (getString YOU_ARE_AT_PICKUP)
    false,true    -> case (getValueToLocalStore LANGUAGE_KEY) of 
      "TA_IN" -> config.customerName <> (getString WAITING_FOR_CUSTOMER)
      "HI_IN" -> "आप" <> config.customerName <> "की प्रतीक्षा कर रहे हैं"
      _       -> (getString WAITING_FOR_CUSTOMER) <> config.customerName