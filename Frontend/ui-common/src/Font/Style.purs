{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Font.Style where


import Common.Types.App
import Data.Eq.Generic (genericEq)
import MerchantConfig.Utils (getValueFromConfig)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Foreign.Generic (class Decode, class Encode)
import Foreign.Generic.EnumEncoding (decodeEnum)
import Halogen.VDom.DOM.Prop (Prop)
import Prelude (class Eq, class Show, Unit, unit, ($), (/=), (==), (<>), (<<<))
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import PrestoDOM (FontWeight(..), fontStyle, lineHeight, textSize, fontWeight)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Types (FontStyle, FontType(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Control.Monad.Except (runExcept)
import Foreign.Generic (Foreign, decode)
import JBridge as JBridge

foreign import getFontStyleFromConfig :: String -> Foreign

getLanguageFromLocalStore :: Unit -> String
getLanguageFromLocalStore _ = JBridge.getKeyInSharedPrefKeys "LANGUAGE_KEY"

getFontType :: String ->  FontType
getFontType dummy = case (decodeFont (getFontStyleFromConfig "")) of
  Just font -> font
  Nothing -> Assets

decodeFont :: Foreign -> Maybe FontType
decodeFont = hush <<< runExcept <<< decode

italic :: FontStyle
italic = do 
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-Italic") (font <> "-Italic") "Arial"

light :: LazyCheck -> FontStyle
light style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Light") (fontKn <> "-Light") "Arial"
    "HI_IN" -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"
    _ -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"

lightItalic :: FontStyle
lightItalic = do 
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-LightItalic") (font <> "-LightItalic") "Arial"

extraLight :: LazyCheck -> FontStyle
extraLight style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-ExtraLight") (fontKn <> "-ExtraLight") "Arial"
    "HI_IN" -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"
    _ -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"

extraLightItalic :: FontStyle
extraLightItalic = do 
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-ExtraLightItalic") (font <> "-ExtraLightItalic") "Arial"

regular :: LazyCheck -> FontStyle
regular style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Regular") (fontKn <> "-Regular") "Arial"
    "HI_IN" -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"
    _ -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"

medium :: LazyCheck -> FontStyle
medium style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Medium") (fontKn <> "-Medium") "Arial"
    "HI_IN" -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"
    _ -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"

mediumItalic :: FontStyle
mediumItalic = do
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-MediumItalic") (font <> "-MediumItalic") "Arial"

semiBold :: LazyCheck -> FontStyle
semiBold style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-SemiBold") (fontKn <> "-SemiBold") "Arial"
    "HI_IN" -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"
    _ -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"

semiBoldItalic :: FontStyle
semiBoldItalic = do 
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-SemiBoldItalic") (font <> "-SemiBoldItalic") "Arial"

bold :: LazyCheck -> FontStyle
bold style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Bold") (fontKn <> "-Bold") "Arial"
    "HI_IN" -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"
    _ -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"

boldItalic :: FontStyle
boldItalic = do 
  let font = getValueFromConfig "fontName"
  fontByOS (font <> "-BoldItalic") (font <> "-BoldItalic") "Arial"

extraBold :: LazyCheck -> FontStyle
extraBold style = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-ExtraBold") (fontKn <> "-ExtraBold") "Arial"
    "HI_IN" -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"
    _ -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"

extraBoldItalic :: FontStyle
extraBoldItalic = do
  let font = getValueFromConfig "fontName"
  let fontKn = getValueFromConfig "fontKannada"
  fontByOS (font <> "-ExtraBoldItalic") (font <> "-ExtraBoldItalic") "Arial"

feFont :: LazyCheck -> FontStyle
feFont style = fontByOS "FEFont" "FE-Font" "Arial"

h1 :: LazyCheck -> forall properties. (Array (Prop properties))
h1 typography = [
  textSize FontSize.a_22
, lineHeight "28"
] <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

h2 :: LazyCheck -> forall properties. (Array (Prop properties))
h2 typography = [
  textSize FontSize.a_18
, lineHeight "23"
] <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

h3 :: LazyCheck ->  forall properties. (Array (Prop properties))
h3 typography = [
   textSize FontSize.a_18
, lineHeight "23"
] <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

subHeading1 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading1 typography = [
  textSize FontSize.a_16
, lineHeight "24"
] <> if (getFontType "")  == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

subHeading2 :: LazyCheck ->  forall properties. (Array (Prop properties))
subHeading2 typography = [
  textSize FontSize.a_16
, lineHeight "24"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body1 ::  LazyCheck -> forall properties. (Array (Prop properties))
body1 typography = [
  textSize $ if (getLanguageFromLocalStore unit) == "TA_IN" then FontSize.a_12 else FontSize.a_14
, lineHeight "18"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body2 :: LazyCheck -> forall properties. (Array (Prop properties))
body2 typography = [
 textSize FontSize.a_14
, lineHeight "20"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body3 ::  LazyCheck -> forall properties. (Array (Prop properties))
body3 typography = [
  textSize FontSize.a_12
, lineHeight "16"
] <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

paragraphText :: LazyCheck -> forall properties. (Array (Prop properties))
paragraphText typography = [
  textSize FontSize.a_14
, lineHeight "18"
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

tags ::  LazyCheck -> forall properties. (Array (Prop properties))
tags typography = [
  textSize FontSize.a_12
, lineHeight "15"
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

captions ::  LazyCheck ->  forall properties. (Array (Prop properties))
captions typography = [
  textSize FontSize.a_10
, lineHeight "13"
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

priceFont ::  LazyCheck -> forall properties. (Array (Prop properties))
priceFont typography = [
    textSize FontSize.a_40
  , lineHeight "40"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

priceFont_big :: LazyCheck -> forall properties. (Array (Prop properties))
priceFont_big typography = [
    textSize FontSize.a_44
  , lineHeight "40"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

fontByOS :: forall a. a -> a -> a -> a
fontByOS android ios web
  = case os of
      "IOS" -> ios
      "WEB" -> web
      _ -> android

body4 ::  LazyCheck -> forall properties. (Array (Prop properties))
body4 typography = [
  textSize FontSize.a_14
, lineHeight "18"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700] 

body5 ::  LazyCheck -> forall properties. (Array (Prop properties))
body5 typography = [
  textSize FontSize.a_16
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body6 ::  LazyCheck -> forall properties. (Array (Prop properties))
body6 typography = [
  textSize FontSize.a_14
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body7 ::  LazyCheck -> forall properties. (Array (Prop properties))
body7 typography = [
  textSize FontSize.a_16
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body8 ::  LazyCheck -> forall properties. (Array (Prop properties))
body8 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body9 ::  LazyCheck -> forall properties. (Array (Prop properties))
body9 typography = [
  lineHeight "22"
, textSize FontSize.a_12
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body10 ::  LazyCheck -> forall properties. (Array (Prop properties))
body10 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body11 ::  LazyCheck -> forall properties. (Array (Prop properties))
body11 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body12 ::  LazyCheck -> forall properties. (Array (Prop properties))
body12 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body13 ::  LazyCheck -> forall properties. (Array (Prop properties))
body13 typography = [
  textSize FontSize.a_18
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body14 ::  LazyCheck -> forall properties. (Array (Prop properties))
body14 typography = [
  textSize FontSize.a_18
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body15 ::  LazyCheck -> forall properties. (Array (Prop properties))
body15 typography = [
  textSize FontSize.a_12
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body16 ::  LazyCheck ->  forall properties. (Array (Prop properties))
body16 typography = [
  textSize FontSize.a_10
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body17 ::  LazyCheck ->  forall properties. (Array (Prop properties))
body17 typography = [
  textSize FontSize.a_10
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body19 ::  LazyCheck ->  forall properties. (Array (Prop properties))
body19 typography = [
  textSize FontSize.a_10
]  <> if getFontType "" == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body18 ::  LazyCheck ->  forall properties. (Array (Prop properties))
body18 typography = [
  textSize FontSize.a_8 
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

h0 :: LazyCheck -> forall properties. (Array (Prop properties))
h0 typography = [
  textSize FontSize.a_24
, lineHeight "28"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

h4 :: LazyCheck ->  forall properties. (Array (Prop properties))
h4 typography = [fontStyle $ feFont LanguageStyle, textSize $ FontSize.a_44]

title0 :: LazyCheck -> forall properties. (Array (Prop properties))
title0 typography = [
  textSize FontSize.a_72
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

title1 :: LazyCheck -> forall properties. (Array (Prop properties))
title1 typography = [
  textSize FontSize.a_28
, lineHeight "40"
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 600]

data Style = Body1
  | Body2
  | Body3
  | Body4
  | Body5
  | Body6
  | Body7
  | Body8
  | Body9
  | Body10
  | Body11
  | Body12
  | Body13
  | Body14
  | Body15
  | Body16
  | Body17
  | Body18
  | Heading0
  | Heading1
  | Heading2
  | Heading3
  | Heading4
  | SubHeading1
  | SubHeading2
  | Tags
  | ParagraphText
  | Captions
  | PriceFont
  | PriceFontBig
  | Title0
  | Title1

getFontStyle :: Style -> LazyCheck -> forall properties. (Array (Prop properties))
getFontStyle style styleType = case style of
  Body1 -> body1 styleType
  Body2 -> body2 styleType
  Body3 -> body3 styleType
  Body4 -> body4 styleType
  Body5 -> body5 styleType
  Body6 -> body6 styleType
  Body7 -> body7 styleType
  Body8 -> body8 styleType
  Body9 -> body9 styleType
  Body10 -> body10 styleType
  Body11 -> body11 styleType
  Body12 -> body12 styleType
  Body13 -> body13 styleType
  Body14 -> body14 styleType
  Body15 -> body15 styleType
  Body16 -> body16 styleType
  Body17 -> body17 styleType
  Body18 -> body18 styleType
  Heading0 -> h0 styleType
  Heading1 -> h1 styleType
  Heading2 -> h2 styleType
  Heading3 -> h3 styleType
  Heading4 -> h4 styleType
  SubHeading1 -> subHeading1 styleType
  SubHeading2 -> subHeading2 styleType
  Tags -> tags styleType
  ParagraphText -> paragraphText styleType
  Captions -> captions styleType
  PriceFont -> priceFont styleType
  PriceFontBig -> priceFont_big styleType
  Title0 -> title0 styleType
  Title1 -> title1 styleType