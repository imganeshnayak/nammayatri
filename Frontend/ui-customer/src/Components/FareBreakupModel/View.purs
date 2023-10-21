{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FareBreakupModel.View where

import Common.Types.App

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, Direction(..))
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.ChooseVehicle.View as ChooseVehicle
import Components.FareBreakupModel.Controller (Action(..), FareBreakupScreenState)
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getPreviousVersion, getSearchType)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import JBridge (getBtnLoader, showKeyboard, getCurrentPosition, firebaseLogEvent, debounceFunction)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Accessiblity(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), FontWeight(..), accessibilityHint ,adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility, fontSize, fontWeight)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (SearchLocationModelType(..), LocationListItemState, RentalConfig(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Data.String as DS

view :: forall w. (Action -> Effect Unit) -> FareBreakupScreenState -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const $ GoBack)
    ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , disableClickFeedback true
        , padding $ Padding 16 16 16 16
        , orientation HORIZONTAL
        , margin $ MarginBottom 16
        ]
        [ imageView
            [ height $ V 24
            , width $ V 24
            , accessibilityHint "Back : Button"
            , accessibility ENABLE
            , imageWithFallback $ "ny_ic_chevron_left" 
            , onClick push (const GoBack)
            ]
        , textView $ [
            text "Rental Ride"
          , margin $ MarginLeft 16
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , margin $ MarginBottom 32
          ]( mapWithIndex
              ( \index item ->
                  if(item.index == item.activeIndex) then ChooseVehicle.view (\action -> pure unit) (item) else emptyTextView config
              ) config.rentalData.quoteList
          )
      , descriptionHeadingView push config "BookingTime"
      , descriptionView push config "BookingTime"
      , descriptionHeadingView push config "BookingDistance"
      , descriptionView push config "BookingDistance"
      , descriptionHeadingView push config "BaseFare"
      , descriptionView push config "BaseFare"
      , descriptionHeadingView push config "TollFee"
      , descriptionView push config "TollFee"
      , noteAndPrimaryButtonView push config
    ]

primaryButtonConfig :: FareBreakupScreenState -> PrimaryButton.Config
primaryButtonConfig config =
  let
    primaryButtonConfig' = PrimaryButton.config
      { textConfig
        { text =  "Confirm"
        , color = Color.yellow900
        , height = V 40
        }
      , height = V config.appConfig.searchLocationConfig.primaryButtonHeight
      , gravity = CENTER
      , cornerRadius = config.appConfig.primaryButtonCornerRadius
      , background = config.appConfig.primaryBackground
      , margin = (MarginHorizontal 16 16)
      , isClickable = true
      , id = "RentalConfirmBooking"
      }
  in primaryButtonConfig'

descriptionHeadingView :: forall w. (Action -> Effect Unit) -> FareBreakupScreenState -> String -> PrestoDOM (Effect Unit) w
descriptionHeadingView push config heading =
  (textView $
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text case heading of
              "BookingTime" -> "Booking on " <> config.rentalData.selectedDate <> ", " <> config.rentalData.selectedTime <> " (" <> config.rentalData.baseDuration <> "hrs)"
              "BookingDistance" -> "Included kms: " <> config.rentalData.baseDuration
              "BaseFare" -> "Non AC Taxi Rental Base Fare: ₹150"
              "TollFee" -> "Tolls and parking fees"
              _ -> ""
    , color Color.black800
    , margin $ MarginHorizontal 16 16
    , fontSize FontSize.a_14
    ]<> FontStyle.body1 TypoGraphy)
 
descriptionView :: forall w. (Action -> Effect Unit) -> FareBreakupScreenState -> String -> PrestoDOM (Effect Unit) w
descriptionView push config description =
  (textView $
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text $ case description of
                "BookingTime" -> "Final fare will be based on the actual trip duration rounded up to the next hour OR the trip duration selected (whichever is higher)."
                "BookingDistance" -> "Any distance covered in excess of the included kms hence, will be charged at ₹18/km."
                "BaseFare" -> "Any additional charges will be billed after your trip is completed."
                "TollFee" -> "Any additional charges will be billed after your trip is completed."
                _ -> ""
    , color Color.black700
    , margin $ Margin 16 4 16 24
    ] <> FontStyle.paragraphText TypoGraphy)

noteAndPrimaryButtonView :: forall w. (Action -> Effect Unit) -> FareBreakupScreenState -> PrestoDOM (Effect Unit) w
noteAndPrimaryButtonView push config =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 0.0
    , gravity BOTTOM
    , margin (MarginBottom 14)
    , onClick push $ const NoAction
    ][ linearLayout 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ Margin 16 0 16 32
      ]
      [ (textView $ [
          text "Note: "
        , color Color.black900
        ] <> FontStyle.body3 TypoGraphy)
        , (textView $ [
            text "A flat night time fee of ₹150 will be charged if your ride starts/ends anytime between 10 PM and 5 AM"
        ] <> FontStyle.body3 TypoGraphy)
      ]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig config)
    ]

emptyTextView :: forall w. FareBreakupScreenState ->  PrestoDOM (Effect Unit) w
emptyTextView config = textView [text "", width $ if os == "IOS" then V 1 else V 0, height $ V 0]