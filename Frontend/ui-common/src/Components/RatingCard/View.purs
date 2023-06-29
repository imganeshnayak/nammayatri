{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RatingCard.View where

import Animation (fadeIn)
import Components.PrimaryButton as PrimaryButton
import Components.RatingCard.Controller (Action(..), RatingCardState)
import Components.SourceToDestination as SourceToDestination
import Data.Array (mapWithIndex, (!!))
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getBtnLoader, getKeyInSharedPrefKeys)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (-), (<<<), (<=), (<>), (==), (<), (/), (/=), not, (&&))
import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, visibility, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, imageUrl, imageView, inputType, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, weight, width, multiLineEditText, pattern, maxLines, editText, imageWithFallback, scrollBarY, scrollView)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Common.Types.App

view :: forall w. (Action -> Effect Unit) -> RatingCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  PrestoAnim.animationSet [ fadeIn true ] $
  relativeLayout
  [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width MATCH_PARENT
      , alignParentBottom "true,-1"
      , clickable true
      , visibility VISIBLE
      , gravity BOTTOM
      , background Color.transparent
      ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding (Padding 16 24 16 30)
        , background Color.white900
        , cornerRadii $ Corners 20.0 true true false false
        , orientation VERTICAL
        , gravity CENTER
        ][  linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , background Color.transparentBlue
            , cornerRadius if os == "IOS" then 25.0 else 60.0
            , orientation HORIZONTAL
            , gravity CENTER
            , padding (Padding 14 14 14 14)
            , margin (MarginBottom 16)
            , visibility if state.props.isItZone then VISIBLE else GONE
            ][ imageView
                [ width (V 20)
                , height (V 20)
                , imageWithFallback "ny_ic_metro_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_metro_blue.png"
                , margin (MarginRight 5)
                ]
              , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text (getString METRO_RIDE)
                , fontStyle $ FontStyle.semiBold LanguageStyle
                , color Color.blue900
                , textSize FontSize.a_14
                ]
              ]
          , (if os == "IOS" && state.props.enableFeedback then scrollView else linearLayout)
            [ width MATCH_PARENT
            , height if os == "IOS" && state.props.enableFeedback then (V 400) else WRAP_CONTENT
            , orientation VERTICAL
            ]([ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER
                , orientation HORIZONTAL
                , visibility GONE
                ][  textView
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , textSize FontSize.a_22
                    , text (getString RIDE_COMPLETED)
                    , color Color.black800
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , gravity CENTER
                    , lineHeight "28"
                    ]
                  ]
              , textView
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , textSize FontSize.a_14
                  , text (getString HOPE_YOUR_RIDE_WAS_HASSLE_FREE)
                  , color Color.black700
                  , fontStyle $ FontStyle.regular LanguageStyle
                  , gravity CENTER
                  , lineHeight "18"
                  , margin (MarginVertical 2 24)
                  , visibility GONE
                  ]
                ] <> (if (not state.props.showFareBreakUp && state.props.isDriver == false) then [horizontalLine state] else [])
                  <> ([starRatingView state push])
                  <> (if state.props.enableFeedback then [editTextView state push] else [])
                  <> ([buttonView push state])
                  )
          ]
        ]
      ]

buttonView :: forall w. (Action -> Effect Unit) -> RatingCardState -> PrestoDOM (Effect Unit) w
buttonView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  ][ --if state.props.currentStage /= HomeScreen then PrimaryButton.view (push <<< SkipButtonAC ) (skipButtonConfig state) else emptyLayout state
     PrimaryButton.view (push <<< PrimaryButtonAC ) (rideRatingButtonConfig state)]

--------------------------------------------------- horizontalLine ---------------------------------------------------

horizontalLine :: forall w. RatingCardState -> PrestoDOM (Effect Unit) w
horizontalLine state =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey900
  , margin $ MarginBottom 24
  , visibility GONE
  ][]

--------------------------------------------------- emptyLayout ---------------------------------------------------

emptyLayout :: forall w. RatingCardState -> PrestoDOM (Effect Unit) w
emptyLayout state =
  linearLayout
  [height $ V 0][]

--------------------------------------------------- editTextView ---------------------------------------------------

editTextView :: forall w. RatingCardState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout
  [ height $ V 94
  , width MATCH_PARENT
  , background Color.grey800
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ MarginBottom 24
  , padding $ Padding 16 16 16 16
  ][  imageView
      [ imageWithFallback "ny_ic_message_square,https://assets.juspay.in/nammayatri/images/common/ny_ic_message_square.png"
      , height $ V 16
      , width $ V 16
      , margin $ if os == "ANDROID" then MarginRight 9 else  Margin 0 6 9 0
      ]
    , (if os == "ANDROID" then editText else multiLineEditText)
      $
      [ height MATCH_PARENT
      , width $ WRAP_CONTENT
      , gravity LEFT
      , padding $ Padding 0 0 0 0
      , textSize FontSize.a_12
      , background Color.grey800
      , color Color.black
      , fontStyle $ FontStyle.regular LanguageStyle
      , hint (getString HELP_US_WITH_YOUR_FEEDBACK)
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine false 
      , onChange push FeedbackChanged  
      ]

  ]

--------------------------------------------------- tripDetailsView ---------------------------------------------------

-- tripDetailsView :: forall w . RatingCardState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- tripDetailsView state push =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , stroke ("1,"<>Color.grey900)
--   , clickable true
--   , cornerRadius 8.0
--   , padding (Padding 16 24 16 24)
--   , margin (MarginBottom 16)
--   , visibility GONE
--   ][( FareBreakUp.view (push <<< FareBreakUpAC ) (fareBreakUpConfig state))]

--------------------------------------------------- rideRatingButtonConfig ---------------------------------------------------

rideRatingButtonConfig :: RatingCardState -> PrimaryButton.Config
rideRatingButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = (getString SUBMIT_FEEDBACK)
        , color = Color.yellow900
        , textSize = FontSize.a_16
        , width = MATCH_PARENT
        }
      , isClickable = if state.ratingCardData.rating < 1 then false else true 
      , alpha = if state.ratingCardData.rating < 1 then 0.4 else 1.0
      , margin = (Margin 0 0 0 0)
      , height = (V 48)
      , gravity = CENTER_VERTICAL
      , cornerRadius = 8.0
      , background = Color.black900
      , id = "RideRatingButton"
      , enableLoader = (getBtnLoader "RightRatingButton")
      }
  in primaryButtonConfig'

--------------------------------------------------- skipRatingButtonConfig ---------------------------------------------------

-- skipButtonConfig :: RatingCardState -> PrimaryButton.Config 
-- skipButtonConfig state = let 
--   config = PrimaryButton.config 
--   skipButtonConfig' = config 
--     { textConfig 
--       { text = (getString SKIP)
--       , color = Color.black700 
--       , fontStyle = FontStyle.bold LanguageStyle
--       , textSize = FontSize.a_16 
--       }
--     , width = V ( screenWidth unit / 4)
--     , background = Color.white900 
--     , stroke = ("1," <> Color.black500)
--     , margin = (MarginRight 12)
--     , id = "SkipCurrentRatingButton"
--     , enableLoader = (getBtnLoader "SkipCurrentRatingButton")
--     }
--   in skipButtonConfig'
  
------------------------starRatingView--------------------------

starRatingView :: forall w . RatingCardState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginBottom 24)
    , gravity CENTER
    , padding (PaddingVertical 16 16)
    , cornerRadius 8.0
    , stroke if state.props.showFareBreakUp then ("1,"<>Color.grey900) else ("0,"<>Color.grey900)
    ][
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation HORIZONTAL
        , margin (MarginBottom 16)
        ][
          textView
            [ height WRAP_CONTENT
            , width $ V (screenWidth unit - 64)
            , textSize FontSize.a_16
            , text (getText state)
            , color Color.black800
            , maxLines 2
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , gravity CENTER_HORIZONTAL
            , lineHeight "20"
            
            ]
          
          , imageView
              [ height $ V 16
              , width $ V 16
              , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
              , onClick push  (const OnClose)
              ]
           ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        ](mapWithIndex (\index item ->
                          linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin (MarginHorizontal 6 6)
                          , onClick push $ const (Rating item)
                          ][imageView
                              [ height $ V 40
                              , width $ V 40
                              , imageWithFallback if item <= state.ratingCardData.rating then "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png" else "ny_ic_star_inactive,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_inactive.png"
                              ]
                          ]) [1,2,3,4,5])
    ]

--------------------------------------------------- fareBreakUpConfig ---------------------------------------------------

-- fareBreakUpConfig :: RatingCardState -> FareBreakUp.Config
-- fareBreakUpConfig state = let
--     config = FareBreakUp.config
--     fareBreakUpConfig' = config
--       {
--           fareDetails = [] -- ToDo :: send the fareDetails from the View file which is using this component.
--         , headingText = (getString VIEW_BREAKDOWN)
--         , totalAmount = {
--             text : (getString TOTAL_AMOUNT)
--             , textSize : FontSize.a_16
--             , fontStyle : FontStyle.semiBold LanguageStyle
--             , color : Color.black800
--             , margin : (Margin 0 0 0 12)
--             , visibility : VISIBLE
--             , priceDetails : {
--                 text : state.ratingCardData.finalAmount
--               , textSize : FontSize.a_16
--               , fontStyle : FontStyle.semiBold LanguageStyle
--               , offeredFare : state.ratingCardData.offeredFare
--               , distanceDifference : state.ratingCardData.distanceDifference
--               }
--             }
--         , rideDetails = {
--               destination : state.ratingCardData.destination
--             , destinationTitle :(fromMaybe "" ((split (Pattern ",") (state.ratingCardData.destination)) !! 0)) 
--             , source :state.ratingCardData.source   
--             , sourceTitle : (fromMaybe "" ((split (Pattern ",") (state.ratingCardData.source)) !! 0))
--             , rideStartTime : state.ratingCardData.rideStartTime
--             , rideStartDate : state.ratingCardData.rideStartDate
--             , estimatedDistance : state.props.estimatedDistance
--         }
--       }
--   in fareBreakUpConfig'

--------------------------------------------------- sourceToDestinationConfig ---------------------------------------------------

sourceToDestinationConfig :: RatingCardState -> SourceToDestination.Config
sourceToDestinationConfig state = let
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = (Margin 2 0 40 0)
    , sourceMargin = (Margin 0 0 0 14)
    , lineMargin = (Margin 19 7 0 0)
    , sourceImageConfig {
        imageUrl = "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
      , height = V 33
      , width = V 33
      , margin = (Margin 4 0 0 0)
      }
    , rideStartedAtConfig {
        text = state.ratingCardData.rideStartTime
      , textSize = FontSize.a_12
      , visibility = VISIBLE
      , padding = (Padding 1 1 1 1)
      , margin = (Margin 5 2 0 0)
      }
    , sourceTextConfig {
        text = state.ratingCardData.source
      , textSize = FontSize.a_14
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 5 0 15 0)
      , fontStyle = FontStyle.medium LanguageStyle
      , ellipsize = true
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl = "ic_location_marker,https://assets.juspay.in/nammayatri/images/user/ic_location_marker.png"
      , height = V 17
      , width = V 14
      , margin = (Margin 13 2 0 0)
      }
    , destinationTextConfig {
        text = state.ratingCardData.destination
      , textSize = FontSize.a_14
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 14 0 15 0)
      , maxLines = 1
      , fontStyle = FontStyle.medium LanguageStyle
      , ellipsize = true
      }
    , rideEndedAtConfig {
        text  = state.ratingCardData.rideEndTime
      , textSize = FontSize.a_12
      , visibility = VISIBLE
      , padding = (Padding 1 1 1 1)
      , margin = (Margin 13 2 0 0)
      }
    }
  in sourceToDestinationConfig'

getText ::  RatingCardState -> String
getText state = let language = (getValueToLocalStore LANGUAGE_KEY)
              in case language of
                          "EN_US" -> ((getString RATE_YOUR_RIDE_WITH) <> state.ratingCardData.driverName )
                          _     ->( state.ratingCardData.driverName <> (getString RATE_YOUR_RIDE_WITH) )
