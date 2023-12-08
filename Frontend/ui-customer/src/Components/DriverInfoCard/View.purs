{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.View where

import Common.Types.App
import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.DriverInfoCard.Controller (Action(..), dummyRideDetails)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String as STR
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl, getPaymentMethod, secondsToHms, makeNumber, getVariantRideType, getTitleConfig)
import Language.Strings (getString)
import Resources.Localizable.EN (getEN)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=),(+), (+))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, shimmerFrameLayout)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..), SearchResultType(..), DriverInfoCardState, RideDetails)
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Storage (KeyStore(..))
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import JBridge(fromMetersToKm)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Helpers.Utils (parseFloat)
import Data.Int(toNumber)
import MerchantConfig.Types (DriverInfoConfig)

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  linearLayout
  [ height WRAP_CONTENT
  , width $ V (screenWidth unit)
  , background Color.transparent
  , orientation VERTICAL
  , id $ getNewIDWithTag "BottomSheetLayout"
  , afterRender push $ const $ NoAction
  ][ driverInfoViewSpecialZone push state
   , driverInfoView push state
   ]

driverInfoViewSpecialZone :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoViewSpecialZone push state =
  linearLayout
  [ width  MATCH_PARENT
  , height WRAP_CONTENT
  , visibility if state.props.currentSearchResultType == QUOTES then VISIBLE else GONE
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.grey700
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][linearLayout
            [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , orientation VERTICAL
            , id $ getNewIDWithTag "driverInfoViewSpecialZone"
            ][ linearLayout
               [ height $ WRAP_CONTENT
               , width $ MATCH_PARENT
               , gravity CENTER
               ][linearLayout
                 [ gravity CENTER
                 , background Color.transparentGrey
                 , height $ V 4
                 , width $ V 34
                 , accessibility ENABLE
                 , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == STATE_EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                 , clickable true
                 , onClick push $ const ToggleBottomSheet
                 , margin (MarginVertical 8 6)
                 , cornerRadius if os == "IOS" then 2.0 else 4.0
                 ][]
               ]
              , titleAndETA push state
              , driverDetailsView push state "SpecialDriverDetailsView" (fromMaybe dummyRideDetails state.data.rideDetails)
              , navigateView push state
              , paymentMethodView push state (getString FARE_ESTIMATE) true "SpecialPaymentMethodView"
            ]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , background Color.grey700
            ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push state
              , cancelRideLayout push state
              , brandingBannerView state.data.config.driverInfoConfig INVISIBLE
            ]
          ]
      ]
  ]

titleAndETA :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
titleAndETA push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , afterRender push $ const $ NoAction
  ][ if state.props.currentStage == RideAccepted then specialZoneHeader (getValueToLocalStore SELECTED_VARIANT)
     else distanceView push state
  ]

specialZoneHeader :: forall w. String -> PrestoDOM ( Effect Unit) w
specialZoneHeader vehicleVariant =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , margin $ MarginTop 6
  , accessibility ENABLE
  , accessibilityHint $ "Board the first" <> (getTitleConfig vehicleVariant).text <>  " from Yatri Sathi zone"
  , accessibility DISABLE_DESCENDANT
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ][  textView $
          [ text $ getString BOARD_THE_FIRST <> " "
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ (getTitleConfig vehicleVariant).text <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if (getValueToLocalStore LANGUAGE_KEY == "ML_IN") then GONE else VISIBLE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL ]
      [ textView $
          [ text $ (getTitleConfig vehicleVariant).text <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if (getValueToLocalStore LANGUAGE_KEY == "ML_IN") then VISIBLE else GONE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ getString $ TAXI_FROM_ZONE "TAXI_FROM_ZONE"
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy]

  ]

navigateView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
navigateView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 44
  , background Color.white900
  , margin $ Margin 16 12 16 0
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , gravity CENTER
  , accessibility ENABLE
  , accessibilityHint $ (getEN $ GO_TO_ZONE "GO_TO_ZONE") <> " : Button"
  , accessibility DISABLE_DESCENDANT
  , visibility if state.props.currentSearchResultType == QUOTES && state.props.currentStage == RideAccepted then VISIBLE else GONE
  , onClick push $ const $ OnNavigateToZone
  ][ imageView
     [ width $ V 20
     , height $ V 20
     , margin $ MarginRight 8
     , imageWithFallback $ fetchImage FF_ASSET "ic_navigation_blue"
     ]
   , textView $ 
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , gravity CENTER
     , text $ getString $ GO_TO_ZONE "GO_TO_ZONE"
     , color Color.blue900
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoView push state =
  let rideData = fromMaybe dummyRideDetails state.data.rideDetails
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility if state.props.currentSearchResultType == QUOTES then GONE else VISIBLE
  , afterRender push $ const $ NoAction
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , background if state.props.zoneType == METRO then Color.blue800 else Color.grey900
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ state.data.config.driverInfoConfig.cardStroke
         ][ linearLayout
            [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , orientation VERTICAL
            ][linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , id $ getNewIDWithTag "driverInfoView"
              ][linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , background Color.blue800
                , cornerRadii $ Corners 24.0 true true false false
                , gravity CENTER
                , orientation HORIZONTAL
                , padding (PaddingVertical 4 4)
                , visibility if state.props.zoneType == METRO then VISIBLE else GONE
                ][imageView
                  [ width (V 15)
                  , height (V 15)
                  , margin (MarginRight 6)
                  , accessibility DISABLE
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_metro_white"
                  ]
                , textView
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_14
                  , accessibility if state.props.zoneType == METRO then ENABLE else DISABLE
                  , accessibilityHint "Metro Ride"
                  , text (getString METRO_RIDE)
                  , color Color.white900
                  ]
                ]
              , linearLayout
                [ orientation VERTICAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                , background Color.grey700
                , gravity CENTER
                , cornerRadii $ Corners 24.0 true true false false
                ][ linearLayout
                  [ gravity CENTER
                  , background Color.transparentGrey
                  , height $ V 4
                  , width $ V 34
                  , accessibility ENABLE
                  , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == STATE_EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                  , margin $ MarginTop 8
                  , clickable true
                  , onClick push $ const ToggleBottomSheet
                  , cornerRadius if os == "IOS" then 2.0 else 4.0
                  ][]
                  , contactView push state
                  , if state.props.currentStage == RideStarted then distanceView push state else dummyView push
                  , driverDetailsView push state "DriverDetailsView"
                  , paymentMethodView push state (getString FARE_ESTIMATE) true "PaymentMethodView"
                ]
              ]
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , background Color.grey700
                ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push state
                , cancelRideLayout push state
                , brandingBannerView state.data.config.driverInfoConfig INVISIBLE
                ]
            ]
         ]
      ]
  ]
  
distanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
distanceView push state = 
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 16 16 16
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , accessibility ENABLE
    , accessibilityHint $ "Arriving at your destination in " <> if (secondsToHms $ fromMaybe 0 state.data.eta) == "--" then "" else (secondsToHms $ fromMaybe 0 state.data.eta) 
    , visibility $ if (state.props.currentStage /= RideAccepted && isJust state.data.eta && (secondsToHms $ fromMaybe 0 state.data.eta) == "--") then GONE else VISIBLE
    ][ textView $
       [ text $ getString REACHING_YOUR_DESTINATION_IN_ <> " "
       , color Color.black800
       , ellipsize true
       , singleLine true
       ] <> FontStyle.subHeading1 TypoGraphy
     , textView $
       [ text $ (secondsToHms $ fromMaybe 0 state.data.eta)
       , color Color.green900
       , ellipsize true
       , singleLine true
       ] <> FontStyle.subHeading1 TypoGraphy
     ]
   , textView $ 
     [text $ getString YOU_HAVE_REACHED_DESTINATION
     , color Color.black800
     , visibility $ if (state.props.currentStage /= RideAccepted && isJust state.data.eta && (secondsToHms $ fromMaybe 0 state.data.eta) == "--") then VISIBLE else GONE
     , singleLine true
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

brandingBannerView :: forall w. DriverInfoConfig -> Visibility -> PrestoDOM (Effect Unit) w
brandingBannerView driverInfoConfig isVisible = 
  let brandingVisibility = if not driverInfoConfig.footerVisibility then GONE else isVisible
  in 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , background driverInfoConfig.footerBackgroundColor
    , padding $ Padding 12 12 12 (12+safeMarginBottom)
    , alignParentBottom "true,-1"
    , visibility $ brandingVisibility
    ][ textView $
        [ text $ getString POWERED_BY 
        , width WRAP_CONTENT    
        , height WRAP_CONTENT
        , color Color.black800
        , padding $ PaddingRight 6
        ] <> FontStyle.body3 TypoGraphy
    , imageView
        [ imageWithFallback $ driverInfoConfig.footerImageUrl
        , width $ V 62
        , height $ V 20
        ]
    ]

cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
cancelRideLayout push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , afterRender push $ const $ NoAction
  , onAnimationEnd push $ const $ NoAction
  , margin $ if state.data.config.showPickUpandDrop then MarginTop 0 else MarginTop 12
  , padding $ PaddingBottom if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0
  , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]) then VISIBLE else GONE
  ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 10 16 10 16
    , accessibilityHint "Cancel Ride : Button"
    , accessibility ENABLE
    , margin $ MarginBottom if os == "IOS" then 24 else 8
    , onClick push $ const $ CancelRide state
    ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black700
      , textFromHtml $ "<u>" <> (getString CANCEL_RIDE) <> "</u>"
      , alpha $ if (getMerchant FunctionCall) == MOBILITY_PM then 0.54 else 1.0
      ] <> FontStyle.body1 TypoGraphy
    ]
  ]

---------------------------------- contactView ---------------------------------------
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> RideDetails -> PrestoDOM (Effect Unit) w
contactView push state rideData =
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , afterRender push $ const $ NoAction
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 20 16 16
    , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]) then if state.data.rideDetails == Nothing then INVISIBLE else VISIBLE else GONE
    ][  linearLayout
        [ width (V (((screenWidth unit)/3 * 2)-27))
        , height WRAP_CONTENT
        , accessibilityHint $ "Ride Status : " <>  if state.data.bookingDetails.distance > 1000 then (state.data.driverName <> " is " <> (secondsToHms (fromMaybe 0 state.data.eta)) <> " and " <> show (toNumber (state.data.bookingDetails.distance) / 1000.0) <> " Kilo Meters Away ") else (rideData.driverName <> if state.data.waitingTime == "--" then " is on the way" else " is waiting for you.")
        , accessibility ENABLE
        , orientation if length rideData.driverName > 16 then VERTICAL else HORIZONTAL
        ][  textView $
            [ text $ rideData.driverName <> " "
            , color Color.black900
            , ellipsize true
            , singleLine true
            ] <> FontStyle.subHeading1 TypoGraphy
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ][ textView $
                [ text $"is " <> ( secondsToHms $ fromMaybe 0 state.data.eta)
                , color Color.black900
                , visibility if (state.data.bookingDetails.distance > 1000 && (secondsToHms $ fromMaybe 0 state.data.eta) /= "--") then VISIBLE else GONE
                ] <> FontStyle.subHeading1 TypoGraphy
              , textView $
                [ text case (state.data.bookingDetails.distance > 1000 && (secondsToHms $ fromMaybe 0 state.data.eta) /= "--") of
                    true -> getString AWAY
                    false -> if state.data.waitingTime == "--" then getString IS_ON_THE_WAY else getString IS_WAITING_AT_PICKUP
                , color Color.black900
                ] <> FontStyle.subHeading1 TypoGraphy
              ]
          ]
      , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        , height WRAP_CONTENT
        ][linearLayout
          [ height $ V 40
          , width $ V 64
          , gravity CENTER
          , cornerRadius if os == "IOS" then 20.0 else 32.0
          , background state.data.config.driverInfoConfig.callBackground
          , stroke state.data.config.driverInfoConfig.callButtonStroke
          , onClick push $ const $ MessageDriver
          , accessibilityHint "Chat and Call : Button"
          , accessibility ENABLE
          ][ imageView
              [ imageWithFallback  $ if (getValueFromConfig "isChatEnabled") == "true" then if state.props.unReadMessages then fetchImage FF_ASSET "ic_chat_badge_green" else fetchImage FF_ASSET "ic_call_msg" else fetchImage FF_COMMON_ASSET "ny_ic_call"
              , height $ V state.data.config.driverInfoConfig.callHeight
              , width $ V state.data.config.driverInfoConfig.callWidth
              ]
          ]
       ]
    ]


---------------------------------- driverDetailsView ---------------------------------------
driverDetailsView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> RideDetails -> PrestoDOM (Effect Unit) w
driverDetailsView push state uid rideData =
 linearLayout
  [ orientation HORIZONTAL
  , height $ V 150
  , padding $ PaddingHorizontal 16 16
  , width MATCH_PARENT
  , afterRender push $ const $ NoAction
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 (if state.props.currentSearchResultType == QUOTES then 12 else 0) 16 0
  , background Color.white900
  , cornerRadius 8.0
  , visibility if state.props.currentSearchResultType == QUOTES then (if state.props.currentStage == RideStarted then VISIBLE else GONE) else if state.data.rideDetails == Nothing then INVISIBLE else VISIBLE
  , gravity BOTTOM
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , alignParentLeft "true,-1"
      ][linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        ][ frameLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ MarginBottom 12
          ][imageView
            [ height $ V 50
            , width $ V 50
            , padding $ Padding 2 3 2 1
            , accessibility if state.data.rating == 0.0 then DISABLE else ENABLE
            , accessibilityHint $ "Driver : Rated " <> show state.data.rating <> " stars"
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_driver"
            ]
            , ratingView push state rideData
            ]
          ]
        , textView $
          [ text rideData.driverName
          , maxLines 1
          , ellipsize true
          , accessibility DISABLE
          , color Color.black800
          , width $ MATCH_PARENT
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.body9 TypoGraphy
        , textView (
          [ text $ rideData.vehicleDetails <> " " 
                    <> case getMerchant FunctionCall of
                          YATRISATHI -> "· " <> getVariantRideType rideData.vehicleVariant
                          _          -> case rideData.vehicleVariant of
                                          "TAXI_PLUS" -> " (" <> (getString AC_TAXI) <> ")"
                                          "TAXI" -> " (" <> (getString NON_AC_TAXI) <> ")"
                                          _ -> ""
          , color Color.black700
          , accessibilityHint $ "Driver : " <> state.data.driverName <> " : Vehicle : " <> case getMerchant FunctionCall of
                                                                                              YATRISATHI -> case state.data.vehicleVariant of
                                                                                                              "TAXI" -> getEN NON_AC_TAXI
                                                                                                              "SUV"  -> getEN AC_SUV
                                                                                                              _      -> getEN AC_CAB
                                                                                              _          -> case state.data.vehicleVariant of
                                                                                                              "TAXI_PLUS" -> (getEN AC_TAXI)
                                                                                                              "TAXI" -> (getEN NON_AC_TAXI)
                                                                                                              _ -> ""
          , accessibility ENABLE
          , width $ V ((screenWidth unit) /2 - 20)
          , maxLines 2
          , singleLine false
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.captions TypoGraphy)
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , accessibility DISABLE
      , gravity RIGHT
      ][  frameLayout
          [ height MATCH_PARENT
          , width $ V 144
          , gravity BOTTOM
          , margin $ MarginBottom 16
          , accessibility DISABLE 
          ][  imageView
              [ imageWithFallback (getVehicleImage state.data.vehicleVariant state.data.vehicleDetails state.props.merchantCity)
              , height $ V 125
              , gravity RIGHT
              , width MATCH_PARENT
              , margin $ MarginBottom 15
              , accessibility DISABLE_DESCENDANT
              ]
            , linearLayout
              [ height $ V 134
              , width MATCH_PARENT
              , gravity BOTTOM
              , accessibility ENABLE
              , accessibilityHint $ "Vehicle Number " <> (STR.replaceAll (STR.Pattern "") (STR.Replacement " ") state.data.registrationNumber)
              ][  linearLayout
                  [ height $ V 38
                  , width MATCH_PARENT
                  , background state.data.config.driverInfoConfig.numberPlateBackground
                  , cornerRadius 4.0
                  , orientation HORIZONTAL
                  , gravity BOTTOM
                  , padding $ Padding 2 2 2 2
                  , alignParentBottom "true,-1"
                  ][
                    linearLayout
                    [ height $ V 34
                    , width MATCH_PARENT
                    , stroke $ "2," <> Color.black
                    , cornerRadius 4.0
                    , orientation HORIZONTAL
                    ][  imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate"
                        , gravity LEFT
                        , visibility if state.data.config.driverInfoConfig.showNumberPlatePrefix then VISIBLE else GONE
                        , background "#1C4188"
                        , height MATCH_PARENT
                        , width $ V 22
                        ]
                        , textView $
                        [ margin $ Margin 2 2 2 2
                        , weight 1.0
                        , height MATCH_PARENT
                        , text $ (makeNumber rideData.registrationNumber)
                        , color Color.black800
                        , fontStyle $ FontStyle.feFont LanguageStyle
                        , gravity CENTER
                        , textSize FontSize.a_14
                        ]
                        , imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate_suffix"
                        , gravity RIGHT
                        , visibility if state.data.config.driverInfoConfig.showNumberPlateSuffix then VISIBLE else GONE
                        , height MATCH_PARENT
                        , width $ V 13
                        ]
                      ]
                    ]
                ]
            ]
        ]
    ]

---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> RideDetails -> PrestoDOM (Effect Unit) w
ratingView push state rideData =
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , afterRender push $ const $ NoAction
  , padding $ Padding 8 3 6 3
  , background state.data.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  state.data.config.driverInfoConfig.ratingStroke
  , cornerRadius state.data.config.driverInfoConfig.ratingCornerRadius
  , accessibility DISABLE
  ][  imageView
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_star_active"
      , height $ V 13
      , width $ V 13
      , accessibility DISABLE
      ]
    , textView $
      [ text $ if rideData.rating == 0.0 then (getString NEW_) else show rideData.rating
      , color state.data.config.driverInfoConfig.ratingTextColor
      , gravity CENTER_VERTICAL
      , margin (Margin 3 0 3 1)
      , textSize $ FontSize.a_10
      , accessibility DISABLE
      ] <> FontStyle.tags TypoGraphy
    ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> Boolean -> String -> PrestoDOM (Effect Unit) w
paymentMethodView push state title shouldShowIcon uid =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , afterRender push $ const $ NoAction
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 12 16 12
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , accessibility ENABLE
  , accessibilityHint $ "Fare Estimate :" <> state.data.config.currency <> show state.data.price <> " : Pay by cash or U P I"
  , cornerRadius 8.0
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility DISABLE_DESCENDANT
      ][  textView $
          [ text title
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ state.data.config.currency <> show state.data.bookingDetails.price
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout
          [ orientation HORIZONTAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , visibility if shouldShowIcon then VISIBLE else GONE
          ][  imageView
              [ imageWithFallback $ fetchImage FF_ASSET  "ny_ic_wallet"
              , height $ V 20
              , width $ V 20
              ]
            , textView $
              [ text $ getString PAY_BY_CASH_OR_UPI
              , color Color.black700
              , padding $ PaddingLeft 4
              ] <> FontStyle.body3 TypoGraphy
            ]
    ]

---------------------------------- tripDetailsView ---------------------------------------

sourceDestinationView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
sourceDestinationView push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 16 0 16 (if os == "IOS" && state.props.currentStage == RideStarted then safeMarginBottom + 36 else 12)
  , background Color.white900
  , afterRender push $ const $ NoAction
  , onAnimationEnd push $ const $ NoAction
  , cornerRadius 8.0
  , padding $ Padding 16 12 16 12
  ][linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , accessibility ENABLE
    , accessibilityHint $ "Pickup : " <> state.data.bookingDetails.source
    ][linearLayout
      [ orientation HORIZONTAL
      , gravity CENTER
      ][imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_pickup"
        , height $ V 8
        , width $ V 8
        ]
        ,textView $
         [ text $ getString PICKUP
         , margin $ MarginLeft 6
         , color Color.black700
         ] <> FontStyle.body3 TypoGraphy
        ]
       , textView $
         [ text state.data.bookingDetails.source
         , maxLines 1
         , ellipsize true
         , width $ V ((screenWidth unit) / 10 * 6)
         , height MATCH_PARENT
         , gravity LEFT
         , color Color.black900
         , margin $ MarginTop 3
         ] <> FontStyle.body1 TypoGraphy
      ]
    , separator (MarginVertical 12 12) (V 1) Color.ghostWhite true
    ,linearLayout
    [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility ENABLE
      , accessibilityHint $ "Drop : " <> state.data.bookingDetails.destination
    ][linearLayout
      [ orientation HORIZONTAL
      , gravity CENTER
      ][imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_drop"
        , height $ V 8
        , width $ V 8
        ]
      , textView $ 
        [ text $ getString DROP
        , margin $ MarginLeft 6
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      ]
      , textView $
        [ text state.data.bookingDetails.destination
        , maxLines 1
        , ellipsize true
        , width $ V ((screenWidth unit) / 10 * 6)
        , height MATCH_PARENT
        , gravity LEFT
        , margin $ MarginTop 3
        , color Color.black900
        ] <> FontStyle.body1 TypoGraphy
     ]
  ]

specialZoneShimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
specialZoneShimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  , visibility VISIBLE
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , linearLayout
      [ height $ V 40
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ customTextView (if state.props.currentStage == RideAccepted then 40 else 20) ((screenWidth unit) / 10 * 6) 0]
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ V 44
        , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
        , margin $ MarginVertical 4 12
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0]
      , if state.props.currentStage == RideStarted then driverInfoShimmer push state else dummyView push
      , paymentMethodShimmer push state
      , addressShimmerView push state
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

shimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
shimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.grey700
  , visibility VISIBLE
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , contactsViewShimmer push state
      , driverInfoShimmer push state
      , paymentMethodShimmer push state
      , addressShimmerView push state
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

customTextView :: forall w. Int -> Int -> Int -> PrestoDOM (Effect Unit) w
customTextView height' width' bottomMargin' =
  shimmerFrameLayout
  [ cornerRadius $ (toNumber height') / 2.0
  , width $ V width'
  , height $ V height'
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , background Color.grey900
    , margin $ MarginBottom bottomMargin'
    , cornerRadius $ (toNumber height') / 2.0
    ][]
  ]

sfl :: forall w. Int -> Int -> Number -> PrestoDOM (Effect Unit) w
sfl height' width' radius' =
  shimmerFrameLayout
  [ cornerRadius radius'
  , stroke $ "1," <> Color.grey900
  , margin $ MarginBottom 12
  , width $ V width'
  , height $ V height'
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , cornerRadius radius'
    , background Color.grey900
    ][]
  ]
contactsViewShimmer :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
contactsViewShimmer push state = 
  linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , padding $ Padding 16 20 16 16
      , visibility if state.data.rideDetails == Nothing then VISIBLE else INVISIBLE
      ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0
       , linearLayout [weight 1.0][]
       , shimmerFrameLayout
         [ height $ V 40
         , width $ V 64
         , cornerRadius 20.0
         , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
         ][linearLayout
           [ height $ V 40
           , width $ V 64
           , cornerRadius 20.0 
           , background Color.grey900
           ][]
         ]
      ]

driverInfoShimmer :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
driverInfoShimmer push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 12 16 12
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , visibility if state.data.rideDetails == Nothing then VISIBLE else INVISIBLE
  ][linearLayout
    [ width $ MATCH_PARENT
    , height $ V 130
    , padding $ Padding 16 16 16 20
    , cornerRadius 8.0
    ][linearLayout
      [ width $ MATCH_PARENT
      , height $ MATCH_PARENT
      , gravity LEFT
      ][linearLayout
        [ height $ MATCH_PARENT
        , gravity LEFT
        , width $ V ((screenWidth unit) / 10 * 4)
        , orientation VERTICAL
        , cornerRadius 8.0
        ][ sfl 40 40 20.0
        , customTextView 16 ((screenWidth unit) / 10 * 4) 4
        , linearLayout
          [ width $ V ((screenWidth unit) / 10 * 3)
          , height $ V 5
          ][]
        , customTextView 13 ((screenWidth unit) / 10 * 3) 0
        ]
      , linearLayout
        [ height $ MATCH_PARENT
        , width $ MATCH_PARENT
        , gravity RIGHT
        ][shimmerFrameLayout
          [ height $ V 90
          , width $ V 130
          , cornerRadius 8.0
          ][linearLayout
            [ height $ V 90
            , width $ V 130
            , cornerRadius 8.0
            , background Color.grey900
            ][]
          ]
        ]
      ]
    ]
  ]

paymentMethodShimmer :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
paymentMethodShimmer push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , margin $ MarginBottom 12
  , padding $ Padding 16 16 16 16
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , gravity CENTER
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width $ V ((screenWidth unit) / 2)
    , orientation VERTICAL
    ][ customTextView 16 80 4
     , customTextView 16 40 4
    ]
   , linearLayout[weight 1.0][]
   , customTextView 16 120 4
  ]

addressShimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
addressShimmerView push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , margin $ MarginBottom 12
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , orientation VERTICAL
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , gravity CENTER_VERTICAL
    ][linearLayout
      [ height $ V 8
      , width $ V 8
      , cornerRadius 52.0
      , background Color.grey900
      , margin $ MarginRight 6
      ][]
    , customTextView 16 40 4
    ]
  , customTextView 20 ((screenWidth unit) / 10 * 6) 4
  , linearLayout
    [ height $ V 1
    , width $ MATCH_PARENT
    , margin $ MarginVertical 12 12
    , background Color.grey900
    ][]
  , linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , gravity CENTER_VERTICAL
    ][linearLayout
      [ height $ V 8
      , width $ V 8
      , cornerRadius 52.0
      , background Color.grey900
      , margin $ MarginRight 6
      ][]
    , customTextView 16 40 4
    ]
  , customTextView 20 ((screenWidth unit) / 10 * 6) 4
  ]

---------------------------------- separator ---------------------------------------

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]

---------------------------------- primaryButtonConfig ---------------------------------------

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let
    config' = PrimaryButton.config
    primaryButtonConfig' = config'
      { width = WRAP_CONTENT
      , height = WRAP_CONTENT
      , background = Color.mint
      , cornerRadius = 17.0
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 18
        , width = V 18
        , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_call"
        , margin = Margin 20 10 20 10
        }
      , id = "CallButton"
      }
  in primaryButtonConfig'


---------------------------------- sourceToDestinationConfig ---------------------------------------

sourceToDestinationConfig :: DriverInfoCardState -> SourceToDestination.Config
sourceToDestinationConfig state = SourceToDestination.config
    {
      margin = Margin 16 0 40 0
    , id = Just "DriverInfoCardSTDC"
    , overrideSeparatorCount = 6
    , separatorMargin = 19
    , sourceImageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_pickup"
      , height = V 14
      , width = V 14
      }
    , sourceTextConfig {
        text = state.data.bookingDetails.source
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , margin = MarginLeft 10
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl =fetchImage FF_ASSET "ny_ic_drop"
      , height = V 14
      , width = V 14
      }
    , destinationTextConfig {
        text = state.data.bookingDetails.destination
      , maxLines = 1
      , textStyle = FontStyle.Body1
      , margin = MarginLeft 10
      , ellipsize = true
      }
    , distanceConfig {
        distanceVisibility = VISIBLE
      , distanceValue = state.data.bookingDetails.estimatedDistance <> " km"
      , background = Color.grey700
  }
    }

destinationView ::  forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
destinationView push state=
  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , margin $ MarginBottom (if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0)
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text if true then "Drop :-" else  getString RIDE_FARE
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.bookingDetails.destination
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 4
            ][ textView (
                [ text $ state.data.bookingDetails.estimatedDistance <> " km"
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
              , linearLayout
                [height $ V 4
                , width $ V 4
                , cornerRadius 2.5
                , background Color.black600
                , margin (Margin 6 2 6 0)
                ][]
              , textView (
                [ text state.props.estimatedTime
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
            ]
      ]
openGoogleMap :: forall w . (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
openGoogleMap push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity LEFT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 20 15 20 15
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][ textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString NAVIGATE)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
        , imageView
          [ width $ V 20
          , height $ V 20
          , margin (MarginLeft 6)
          , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_navigation"
          ]
      ]
  ]

dummyView :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push  =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

configurations ∷ { letterSpacing ∷ Number , paddingOTP ∷ Padding , paddingOTPText ∷ Padding }
configurations =
  case os of
    "IOS" -> {paddingOTPText : PaddingVertical 4 4
              , letterSpacing : 6.0
              , paddingOTP : Padding 20 5 18 7}
    _     -> {paddingOTPText : PaddingVertical 2 2
              , letterSpacing : 3.0
              , paddingOTP : Padding 11 0 11 7
              }

getVehicleImage :: String -> String -> Maybe String -> String
getVehicleImage variant vehicleDetail city = do
  let details = (toLower vehicleDetail)
  fetchImage FF_ASSET $ 
    if variant == "AUTO_RICKSHAW" then maybe "ic_auto_rickshaw" mkVehicleImage city
    else
      if contains (Pattern "ambassador") details then "ic_yellow_ambassador"
      else 
        case (getMerchant FunctionCall) of
          YATRISATHI -> case variant of
                          "SUV" -> "ny_ic_suv_concept"
                          _     -> "ny_ic_sedan_concept"
          _          -> "ic_white_taxi"
    where 
      mkVehicleImage :: String -> String
      mkVehicleImage cityCode = 
        if cityCode == "std:040" then "ic_auto_rickshaw_black_yellow"
          else "ic_auto_rickshaw"

getAnimationDelay :: LazyCheck -> Int
getAnimationDelay dummy = 100
