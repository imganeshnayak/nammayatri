{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.MessagingView.Controller where

import MerchantConfig.Types

import MerchantConfig.DefaultConfig as DC
import Screens.Types (SheetState(..))

data Action = SendMessage
            | SendSuggestion String
            | BackPressed
            | TextChanged String
            | EnableSuggestions
            | Call
            | NoAction
            | ScrollStateChanged String
            | OnSlide Number

type Config = 
  { userConfig :: UserConfig
  , messages :: Array ChatComponent
  , messagesSize :: String
  , sendMessageActive :: Boolean
  , vehicleNo :: String
  , suggestionsList :: Array String
  , suggestionDelay :: Int
  , spanParent :: Boolean
  , showTextEdit :: Boolean
  , hint :: String
  , showHeader :: Boolean
  , showStroke :: Boolean
  , languageKey :: String
  , enableSuggestionClick :: Boolean
  , canSendSuggestion :: Boolean
  , enableCall :: Boolean
  , showAutoGeneratedText :: Boolean
  , rideConfirmedAt :: String
  , autoGeneratedText :: String
  , driverRating :: String
  , fareAmount :: String
  , config :: AppConfig
  , chatSheetState :: SheetState
  , peekHeight :: Int
  , chatSheetSlide :: Number
  , isExpanding :: Boolean
  , isKeyboardOpen :: Boolean
  , sheetState :: SheetState
  }

type UserConfig =
  { userName :: String
  , appType :: String
  }

type ChatComponent = {
    message :: String 
  , sentBy :: String 
  , timeStamp :: String
  , type :: String
  , delay :: Int
}

config :: Config
config = 
  {
    userConfig : 
        {
          userName : ""
        , appType : ""
        }
    , messages : []
    , messagesSize : ""
    , sendMessageActive : false
    , vehicleNo : ""
    , suggestionsList : []
    , hint : ""
    , suggestionDelay : 0
    , spanParent : false
    , languageKey : ""
    , enableSuggestionClick : true
    , showHeader : true
    , showStroke : true
    , showTextEdit : true
    , canSendSuggestion : true
    , enableCall : true 
    , showAutoGeneratedText : false
    , rideConfirmedAt : ""
    , autoGeneratedText : ""
    , driverRating : ""
    , fareAmount : ""
    , config : DC.config
    , chatSheetState : STATE_COLLAPSED
    , peekHeight : 0
    , chatSheetSlide : 0.0
    , isExpanding : true
    , isKeyboardOpen : false
    , sheetState : STATE_COLLAPSED
  }

makeChatComponent :: String -> String -> String -> ChatComponent
makeChatComponent message sender timeStamp =  {
  "message" : message
, "sentBy" : sender
, "timeStamp" : timeStamp
, "type" : "Text"
, delay : 0
}

