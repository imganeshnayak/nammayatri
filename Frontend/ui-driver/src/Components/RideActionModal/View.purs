{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RideActionModal.View  where

import Common.Types.App
import PrestoDOM.Animation as PrestoAnim
import Animation (scaleYAnimWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.RideActionModal.Controller (Action(..), Config)
import Components.SeparatorView.View as SeparatorView
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (screenWidth, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (countDown, getRideLabelData, getRequiredTag, clearTimer, getCurrentUTC, getCommonAssetStoreLink, getAssetStoreLink)
import JBridge (getVersionCode, waitingCountdownTimer, toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils (getMerchant, getValueFromConfig, Merchant(..))
import Prelude ((<>))
import Prelude (Unit, bind, const, not, discard, pure, show, unit, ($), (/=), (<>), (&&), (==), (-), (>), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, ellipsize, fontSize, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, pivotY, onAnimationEnd, id)
import PrestoDOM.Properties (cornerRadii, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (HomeScreenStage(..), TimerStatus(..), DisabilityType(..), RideType(..))
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import Helpers.Utils(getRideTypeColor, getVariantRideType)
import JBridge as JB

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , id $ getNewIDWithTag "rideActionHeaderLayout"
        , padding $ PaddingBottom 16
        ][  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            ][ messageButton push config
            , callButton push config
            , openGoogleMap push config
            ]
          ]
        ]
    , if isSpecialRide config
        then rideActionViewWithLabel push config else rideActionView (MarginTop 0) push config
    ]


messageButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
messageButton push config =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) && checkVersionForChat (getCurrentAndroidVersion (getMerchant FunctionCall)) then VISIBLE else GONE
  , padding $ Padding 20 16 20 16
  , margin $ MarginLeft 16
  , background Color.white900
  , stroke $ "1,"<> Color.black500
  , cornerRadius 30.0
  , afterRender push $ const $ LoadMessages
  , onClick push $ const $  if config.accessibilityTag == Maybe.Just BLIND_AND_LOW_VISION then VisuallyImpairedCustomer else MessageCustomer
  , alpha if config.accessibilityTag == Maybe.Just BLIND_AND_LOW_VISION then 0.5 else 1.0
  , clickable true
  ][  imageView
      [ imageWithFallback if config.unReadMessages then "ic_chat_badge," <> (getCommonAssetStoreLink FunctionCall) <> "ic_chat_badge.png" else "ic_chat," <> (getCommonAssetStoreLink FunctionCall) <> "ic_chat.png"
      , height $ V 20
      , width $ V 20
      ]
  ]

getCurrentAndroidVersion :: Merchant -> Int
getCurrentAndroidVersion merchant =
  case merchant of
    NAMMAYATRI -> 54
    YATRI -> 47
    YATRISATHI -> 1
    _ -> 1

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
  , alpha if config.accessibilityTag == Maybe.Just HEAR_IMPAIRMENT then 0.5 else 1.0
  , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) then VISIBLE else GONE
  , onClick push (const $ CallCustomer)
  , clickable (not (config.accessibilityTag == Maybe.Just HEAR_IMPAIRMENT))
  ][  imageView
      [ imageWithFallback $ "ic_phone," <> (getCommonAssetStoreLink FunctionCall) <> "/ic_phone.png"
      , height $ V 20
      , width $ V 20
      ]
  ]
  
rideActionViewWithLabel :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit) w
rideActionViewWithLabel push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background $ getRideLabelData "backgroundColor" config.specialLocationTag config.accessibilityTag 
  , cornerRadii $ Corners 25.0 true true false false
  , orientation VERTICAL
  , padding $ PaddingTop 5
  , gravity CENTER
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , id $ getNewIDWithTag "rideActionLabelLayout"
      ][ imageView
          [ width $ V 18
          , height $ V 18
          , imageWithFallback $ getRideLabelData "imageUrl" config.specialLocationTag config.accessibilityTag 
          ]
        , textView $
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , text $ getRideLabelData "text" config.specialLocationTag config.accessibilityTag 
          , gravity CENTER_VERTICAL
          , color Color.white900
          , margin $ MarginLeft 5
          ] <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
        , textView $ 
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , text "|"
          , visibility if Maybe.isJust config.accessibilityTag then VISIBLE else GONE
          , gravity CENTER_VERTICAL
          , color Color.white900
          , margin $ MarginLeft 5
          ] <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginLeft 5
          , visibility if Maybe.isJust config.accessibilityTag then VISIBLE else GONE
          , onClick push $ const SecondaryTextClick
          ]
          [ textView $ 
              [ width WRAP_CONTENT
              , height MATCH_PARENT
              , text $ getRideLabelData "secondaryText" config.specialLocationTag config.accessibilityTag 
              , gravity CENTER_VERTICAL
              , color Color.white900
              ] <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
          , linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , background Color.white900
              , margin (Margin 1 0 2 0)
              ][]
          ]
      ]
    , rideActionView (MarginTop 6) push config
  ]

rideTypeView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideTypeView push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , visibility if config.appConfig.rideActionModelConfig.showVehicleVariant && config.requestedVehicleVariant /= Maybe.Nothing then VISIBLE else GONE
  ][ linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey800
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 16
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString RIDE_TYPE <> ":"
          , color Color.black650
          ] <> FontStyle.body1 TypoGraphy
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ case config.requestedVehicleVariant of
                      Maybe.Just variant -> getVariantRideType variant
                      Maybe.Nothing      -> ""
          , margin $ MarginLeft 8
          , color $ getRideTypeColor config.requestedVehicleVariant
          ] <> FontStyle.body1 TypoGraphy
      ]
  ]

rideActionView :: forall w . Margin -> (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionView layoutMargin push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadii $ Corners 25.0 true true false false
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingTop 6
  , gravity CENTER
  , margin layoutMargin
  , stroke $ "1," <> Color.grey800
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , id $ getNewIDWithTag "rideActionLayout"
      ][  rideActionDataView push config
        , rideTypeView push config
        , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.lightGrey
          , margin $ MarginTop 24
          ][]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , padding $ Padding 16 16 16 24
          ][ if config.startRideActive then startRide push config else endRide push config]
        ]
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
          , imageWithFallback $ "ny_ic_navigation," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_navigation.png"
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
            , width MATCH_PARENT
            , orientation VERTICAL
            ][  rideInfoView push config
              , if config.rideType == RENTAL_BOOKING then 
                  if config.startRideActive then locationView config push "source" else rentalRideDescView config push 
                else 
                  if config.startRideActive then sourceAndDestinationView push config else locationView config push "destination"
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
        , text $ if config.rideType == RENTAL_BOOKING then (getString DISTANCE) else (getString RIDE_DISTANCE)
        , color Color.black650
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.totalDistance
        , color Color.black900
        , ellipsize true
        , singleLine true
        ] <> FontStyle.h3 TypoGraphy
    ]

totalDurationView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
totalDurationView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity START
    , orientation VERTICAL
    , weight 1.0
    ][ textView $
       [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString DURATION)
        , color Color.black650
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ] $ [  textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text if config.startRideActive then config.totalDuration else config.durationTravelled
              , color Color.black900
              , ellipsize true
              , singleLine true
              ] <> FontStyle.h3 TypoGraphy
            ] <> if config.startRideActive then [] 
                  else [  textView $
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , text (" / " <> config.totalDuration)
                          , color Color.black650
                          , ellipsize true
                          , singleLine true
                          ] <> FontStyle.h3 TypoGraphy
                        ]
    ]

sourceAndDestinationView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceAndDestinationView push config =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 24 24
    , afterRender push $ const NoAction
    ][  sourceDestinationImageView config { srcImage : "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png" 
                                          , destImage : "ny_ic_destination," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_destination.png"}
      , sourceDestinationTextView push config
      ]

startRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
startRide push config =
  PrestoAnim.animationSet
  [ scaleYAnimWithDelay (getAnimationDelay config)
  ]$ linearLayout
  [ width MATCH_PARENT
  , height (V 50)
  , background Color.darkMint
  , cornerRadius 8.0
  , gravity CENTER
  , onClick push (const $ StartRide)
  , pivotY 0.0
  , onAnimationEnd push $ const NoAction
  , afterRender push $ const NoAction
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString START_RIDE)
      , color Color.white900
      , afterRender push $ const NoAction
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
  , gravity CENTER
  , onClick push (const $ EndRide)
  , afterRender push $ const NoAction
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString END_RIDE)
      , color Color.white900
      , padding (Padding 0 0 0 4)
      , afterRender push $ const NoAction
      ] <> FontStyle.subHeading1 TypoGraphy
      )
  ]

cancelRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cancelRide push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , background Color.white900
  , visibility if config.startRideActive then VISIBLE else GONE
  , padding $ PaddingBottom 16
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
      ]$[  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getTitle config
          , color Color.greyTextColor
          , ellipsize true
          , singleLine false
          ] <> FontStyle.subHeading2 TypoGraphy
        ] <> if config.isDriverArrived then [arrivedButtonView push config] else [dummyView push]
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
        , text if config.rideType == RENTAL_BOOKING then "Rental Fare" else (getString RIDE_FARE)
        , color Color.black650
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text ((getValueFromConfig "currency") <> (show config.estimatedRideFare))
        , color Color.black900
        , ellipsize true
        , singleLine true
        ] <> FontStyle.h3 TypoGraphy
    ]

waitTimeView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
waitTimeView push config =
   linearLayout
     [ height WRAP_CONTENT
     , gravity START
     , orientation VERTICAL
     , weight 1.0
     , visibility if isWaitTimeVisible config then VISIBLE else GONE
     ]
     [ linearLayout
         [
          orientation HORIZONTAL
         ]
         [textView $
        [ height WRAP_CONTENT
         , width $ V 80
         , text (getString WAIT_TIME)
         , color Color.black650
         , textSize FontSize.a_14
         , ellipsize true
         , singleLine true
         ] <> FontStyle.body1 TypoGraphy
        ,
        imageView
          [ height MATCH_PARENT
            , width  $ V 25
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
    , afterRender push $ const NoAction
    ] $ [] 
      <>  if config.rideType == RENTAL_BOOKING then rentalRideOrder config push else normalRideOrder config push
      <>  [ linearLayout
            [ weight 1.0
            , height MATCH_PARENT
            ][]
          ]
  

normalRideOrder ::  Config -> (Action -> Effect Unit) -> forall w. Array (PrestoDOM (Effect Unit) w) 
normalRideOrder config push = 
  [ estimatedFareView push config 
  , separator true
  , totalDistanceView push config
  ] <> if (isWaitTimeVisible config) 
        then 
        [ separator true
        , waitTimeView push config
        ] else []

rentalRideOrder :: Config -> (Action -> Effect Unit) -> forall w. Array (PrestoDOM (Effect Unit) w) 
rentalRideOrder config push = 
  [ estimatedFareView push config]
  <> if config.startRideActive then 
      [ separator true
      , totalDurationView push config 
      , separator true
      , if (isWaitTimeVisible config) then waitTimeView push config else totalDistanceView push config 
      ] 
      else 
      [ separator true
      , totalDurationView push config
      ]
    

separator :: forall w . Boolean -> PrestoDOM (Effect Unit) w
separator visibility' =
  linearLayout
    [ width $ V 2
    , height MATCH_PARENT
    , margin $ MarginHorizontal 12 12
    , visibility if visibility' then VISIBLE else GONE
    ][ linearLayout
      [ width $ V 1
      , background Color.lightGrey
      , height MATCH_PARENT
      ][]
    ]

sourceDestinationImageView :: forall w . Config -> {srcImage :: String , destImage :: String } -> PrestoDOM (Effect Unit) w
sourceDestinationImageView config imageConfig =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginLeft 4
    , orientation VERTICAL
    ][ imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageWithFallback $ imageConfig.srcImage
        ]
      , SeparatorView.view separatorConfig
      , imageView
        [ height $ V 14
        , width $ V 14
        , imageWithFallback $ imageConfig.destImage
        ]
      ]


sourceDestinationTextView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceDestinationTextView push config =
  linearLayout
    [ width WRAP_CONTENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    , afterRender push $ const NoAction
    ][  addressTextView config push "source" (MarginLeft 0)
      , addressTextView config push "destination" (MarginLeft 0)
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
  , afterRender push $ const NoAction
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
      , imageWithFallback if config.notifiedCustomer then "ny_ic_tick_grey," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_tick_grey.png" else "ny_ic_hand_wave," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_hand_wave.png"
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

locationView :: forall w . Config -> (Action -> Effect Unit) -> String -> PrestoDOM (Effect Unit) w
locationView config push locType =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 24 24
  ][  imageView
      [ height $ V if locType == "source" then 14 else 24
      , width $ V if locType == "source" then 14 else 24
      , imageWithFallback $ if locType == "source" then "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png" else "ny_ic_loc_red," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_loc_red.png"
      , margin $ Margin 0 (if locType == "source" then 7 else 3) 8 0
      ]
    ,  addressTextView config push locType (MarginLeft 0)
  ]

lineImageView :: forall w . Int -> PrestoDOM (Effect Unit) w
lineImageView val =
  imageView
    [ height $ V val
    , width $ V 15
    , imageUrl "ic_line"
    , margin $ MarginLeft 7
    ]

dummyView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push =
  textView
    [ afterRender push $ const NoAction
    , width $ V 0
    , height $ V 0
    ]

addressTextView :: forall w . Config -> (Action -> Effect Unit) -> String -> Margin -> PrestoDOM (Effect Unit) w
addressTextView config push locType marginVal =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ marginVal
    , orientation VERTICAL
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ if locType == "source" then config.sourceAddress.titleText else config.destinationAddress.titleText
        , id (getNewIDWithTag "locationArea")
        , color Color.black800
        , ellipsize true
        , singleLine true
        ] <> FontStyle.subHeading1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ if locType == "source" then config.sourceAddress.detailText else config.destinationAddress.detailText
        , id (getNewIDWithTag "locationAddress")
        , color Color.black650
        , ellipsize true
        , maxLines if config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer then 1 else 2
        ]<> FontStyle.body1 TypoGraphy
      ]
  
rentalRideDescView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rentalRideDescView config push = 
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , afterRender push $ const NoAction
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , margin (MarginLeft 25)
          , width MATCH_PARENT
          ][  textView $ 
              [ text $ (getString START_TIME) <> ": " 
              , height WRAP_CONTENT
              , width WRAP_CONTENT
              , color Color.black700
              ] <> FontStyle.body1 TypoGraphy
            , textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , color Color.black800
              , text config.startTime
              ] <> FontStyle.body1 TypoGraphy
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 25 8 0 0)
        ][  textView $ 
            [ text $ (getString START_ODO_READING) <> ": "
            , height WRAP_CONTENT
            , width WRAP_CONTENT
            , color Color.black700
            ] <> FontStyle.body1 TypoGraphy
          , textView $ 
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , color Color.black800
            , text $ config.startODOReading <> " Kms"
            ] <> FontStyle.body1 TypoGraphy
          ]
      , pickUpAtView config push
      ] 
  , sourceDestinationImageView config { srcImage : "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png" 
                                      , destImage : "ny_ic_drop_indicator," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_drop_indicator.png"}
  ]

pickUpAtView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pickUpAtView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][ linearLayout
      [height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ Margin 0 4 0 0
      , cornerRadius 16.0
      , padding $ Padding 24 3 16 5
      , background "#F2F2F4"][
        textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString PICKED_UP_AT)
          , color Color.black600
          ] <> FontStyle.body1 TypoGraphy
      ]
    , addressTextView config push "source" (MarginLeft 24)

  ]

getTitle :: Config -> String
getTitle config = case config.startRideActive of
  false -> if config.rideType == RENTAL_BOOKING then ((getString YOU_ARE_ON_A_RENTAL_RIDE) <> "..." )else (getString YOU_ARE_ON_A_RIDE)
  true  -> case config.isDriverArrived, config.notifiedCustomer of
    false, false  -> (config.customerName <> " " <> (getString IS_WAITING_FOR_YOU) <> "...")
    true, _       -> (getString YOU_ARE_AT_PICKUP)
    false,true    -> case (getValueToLocalStore LANGUAGE_KEY) of
      "TA_IN" -> config.customerName <> (getString WAITING_FOR_CUSTOMER)
      "HI_IN" -> "आप" <> config.customerName <> "की प्रतीक्षा कर रहे हैं"
      _       -> (getString WAITING_FOR_CUSTOMER) <> config.customerName


separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation : VERTICAL
  , count : 6
  , height : V 4
  , width : V 2
  , layoutWidth : V 14
  , layoutHeight : V 16
  }

isSpecialRide :: Config -> Boolean
isSpecialRide config = ((Maybe.isJust config.specialLocationTag) || (Maybe.isJust config.accessibilityTag)) && Maybe.isJust (getRequiredTag "text" config.specialLocationTag config.accessibilityTag)

getAnimationDelay :: Config -> Int
getAnimationDelay config = 50

isWaitTimeVisible :: Config -> Boolean
isWaitTimeVisible config = (config.waitTime /= "__") && (config.notifiedCustomer) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "NoView" ) && ((getValueToLocalStore IS_WAIT_TIMER_STOP) /= "Stop" )