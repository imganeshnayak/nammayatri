{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.AddNewAddressScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Engineering.Helpers.Commons as EHC 
import Data.Maybe (Maybe(..))
import Data.String as DS 
import Language.Strings (getString)
import Language.Types (STR(..))
import JBridge as JB 
import Screens.Types as ST
import Styles.Colors as Color
import Prelude (not, (&&), (+), (<>), (==), (||))
import PrestoDOM (Length(..), Margin(..), Visibility(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))

primaryButtonConfigConfirmLoc :: ST.AddNewAddressScreenState -> PrimaryButton.Config 
primaryButtonConfigConfirmLoc state = let 
  config = PrimaryButton.config 
  primaryButtonConfig' = config 
    { textConfig 
      { text = (getString CONFIRM_LOCATION)
      , color = state.data.config.primaryTextColor
      }
    , background = state.data.config.primaryBackground
    , margin = (Margin 0 22 0 16)
    , id = "AddNewaddressConfirmLocationButton"
    }
  in primaryButtonConfig'

genericHeaderConfig :: ST.AddNewAddressScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
        height = (V 25)
      , width = (V 25)
      , margin = (Margin 10 17 16 15)
      , visibility = VISIBLE
      , imageUrl = if state.data.config.nyBrandingVisibility && (not state.props.showSavePlaceView) then config.prefixImageConfig.imageUrl 
                    else if state.props.showSavePlaceView then if state.data.config.nyBrandingVisibility then "ny_ic_close,"<> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_close.png"
                      else  "ny_ic_close_white," <> (getAssetStoreLink FunctionCall) <> "ny_ic_close_white.png" else config.prefixImageConfig.imageUrl
      }
    , textConfig {
        text = if state.props.showSavePlaceView then (getString FAVOURITE_LOCATION) else if state.props.editLocation then (getString EDIT_FAVOURITE) else (getString ADD_FAVOURITE)
      , color = state.data.config.quoteListModel.textColor
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryEditTextConfig :: ST.AddNewAddressScreenState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , singleLine = true
        , placeholder = (getString GIVE_THIS_LOCATION_A_NAME) 
        , textStyle = FontStyle.Body1
        , pattern = Just "[a-zA-Z0-9'‘’. ]*,30"
        , text = if (DS.toLower state.data.placeName == "home" || DS.toLower state.data.placeName == "work" ) then "" else state.data.placeName
        }
      , background = Color.white900
      , topLabel
        { text = (getString SAVE_AS)
        , color = Color.black800
        , textStyle = FontStyle.Tags
        }
      , stroke = ("1,"<> Color.black500)
      , margin = (Margin 0 0 0 0)
      , id = (EHC.getNewIDWithTag "SaveAsEditText")
      , errorLabel 
        { text = (getString NAME_ALREADY_IN_USE)
        , margin = (MarginTop 1)
        }
      , showErrorLabel = state.props.placeNameExists
      }
    in primaryEditTextConfig'

primaryButtonConfig :: ST.AddNewAddressScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if (state.props.editSavedLocation) then (getString CONFIRM_CHANGES) else (getString CONFIRM_AND_SAVE)
      , color = state.data.config.primaryTextColor }
      , margin = MarginBottom 24
      , isClickable = (state.props.isBtnActive && state.props.isLocationServiceable && (not state.props.tagExists))
      , alpha = if (state.props.isBtnActive && state.props.isLocationServiceable && (not state.props.tagExists)) then 1.0 else 0.4
      , id = "AddNewAddressButton"
      , enableLoader = (JB.getBtnLoader "AddNewAddressButton")
      , background = state.data.config.primaryBackground
      }
  in primaryButtonConfig'