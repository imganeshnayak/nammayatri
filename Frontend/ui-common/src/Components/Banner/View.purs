{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Banner.View where

import Prelude
import Effect (Effect)
import PrestoDOM ( Margin(..), Orientation(..), Padding(..), Visibility(..), Length(..), PrestoDOM, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, text, textFromHtml, textSize, textView, weight, width, padding, visibility, afterRender, editText, onClick, alignParentBottom, imageWithFallback, stroke, layoutGravity )
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Font.Style as FontStyle
import Font.Size as FontSize
import Components.Banner.Controller
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Commons (os, screenWidth)


view :: forall w. (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 12.0
    , margin config.margin
    , padding config.padding
    , background config.backgroundColor
    , visibility if config.isBanner then VISIBLE else GONE
    , gravity CENTER_VERTICAL
    , onClick push (const OnClick)
    , clickable config.bannerClickable
    , stroke config.stroke
    ]
    [  linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , padding $ Padding 20 0 0 0
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        ]
        [ 
          textView $
          [ height if config.alertTextVisibility then WRAP_CONTENT else V 0
          , width MATCH_PARENT
          , gravity LEFT
          , text config.alertText
          , color config.alertTextColor
          , padding $ PaddingBottom 2
          , visibility if config.alertTextVisibility then VISIBLE else GONE
          ] <> (FontStyle.getFontStyle config.alertTextStyle LanguageStyle)
        , textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity LEFT
          , text config.title
          , color config.titleColor
          , padding $ PaddingBottom 2
          , visibility if config.titleTextVisibility then VISIBLE else GONE
          ] <> (FontStyle.getFontStyle config.titleStyle LanguageStyle)
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , visibility if config.actionTextVisibility then VISIBLE else GONE
          ]
          [
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity LEFT
            , text config.actionText
            , color config.actionTextColor
            , padding $ PaddingBottom 2
            ] <> (FontStyle.getFontStyle config.actionTextStyle LanguageStyle)
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity LEFT
            , textFromHtml "&rarr;"
            , color config.actionTextColor
            , padding $ PaddingBottom 3
            , margin $ MarginLeft 5
            , visibility if config.showActionArrow then VISIBLE else GONE
            ] <> (FontStyle.getFontStyle config.actionTextStyle LanguageStyle)
          ]
        ]
    ,   linearLayout
        [
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , padding $ config.imagePadding
        ][imageView
            [
              height config.imageHeight
            , width config.imageWidth
            , margin $ MarginRight 5
            , imageWithFallback config.imageUrl
            ]]
    ]