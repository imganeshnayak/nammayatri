module Screens.BookingOptionsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ChooseVehicle as ChooseVehicle
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils ( getVehicleType, fetchImage, FetchImageFrom(..), getVariantRideType, getVehicleVariantImage, getDowngradeOptionsText, getUIDowngradeOptions)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, ($), (<<<), (<>), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine)
import Screens.BookingOptionsScreen.Controller (Action(..), ScreenOutput, eval, getVehicleCapacity)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Prelude ((<>))
import Engineering.Helpers.Utils (capitalizeFirstChar)

screen :: ST.BookingOptionsScreenState -> Screen Action ST.BookingOptionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BookingDetailsScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let
            _ = spy "BookingOptionsScreenState -----" state
          let
            _ = spy "BookingOptionsScreenState--------action" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  , padding $ PaddingBottom 24
  ] $ [ headerLayout push state
      , defaultVehicleView push state
      , downgradeVehicleView push state
      , linearLayout
        [ height MATCH_PARENT
        , width $ V 1
        , weight 1.0
        ] []
      ]

downgradeVehicleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
downgradeVehicleView push state =
  let canDowngrade = not $ DA.null state.data.downgradeOptions
      downgradeFrom = case state.data.vehicleType of
                        "SUV" -> getString AC_SUV
                        _     -> getString AC_CAB
      downgradeTo = case state.data.vehicleType of
                      "SUV" -> getString AC_CAB
                      _     -> getString TAXI
  in  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginHorizontal 16 16)
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      , orientation VERTICAL
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , margin (Margin 16 16 16 16)
          ][  textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color $ if canDowngrade then Color.black800 else Color.black600
              , text $ getString DOWNGRADE_VEHICLE
              ] <> FontStyle.body4 TypoGraphy
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity RIGHT
              ][ toggleView push state.props.downgraded canDowngrade ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , margin (Margin 16 0 16 16)
          , background Color.grey700
          ][]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin 16 0 16 16)
          , padding (Padding 16 16 16 16)
          , orientation HORIZONTAL
          , background Color.linen
          , cornerRadius 8.0
          , visibility if canDowngrade then GONE else VISIBLE
          ][  imageView
              [ width $ V 15
              , height $ V 15
              , margin (Margin 0 3 8 0)
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_orange"
              ]
            , textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text (getString DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES)
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy  
           ]
        , textView $
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin 16 0 16 20)
          , text $ getDowngradeOptionsText state.data.vehicleType
          , color Color.black700
          , visibility if canDowngrade then VISIBLE else GONE
          ] <> FontStyle.body1 TypoGraphy 
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin (Margin 12 0 12 16)
          , visibility if canDowngrade then VISIBLE else GONE
          ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ] $ [  downgradeVehicleCard state.data.vehicleType true true
                  ] <>  ( map
                          ( \item ->
                            linearLayout
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              ][downgradeVehicleCard item state.props.downgraded false
                                  ]) $ (getUIDowngradeOptions state.data.vehicleType)
                            )
            ]
      ]

downgradeVehicleCard :: forall w. String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
downgradeVehicleCard variant enabled opacity =
  frameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , margin (MarginHorizontal 2 2)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 12 12 12 12)
      , margin (Margin 2 5 2 5)
      , orientation HORIZONTAL
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      , gravity CENTER_VERTICAL
      ][  imageView
          [ imageWithFallback $ getVehicleVariantImage variant
          , width $ V 35
          , height $ V 35
          ]
        , textView
          [ weight 1.0
          , height WRAP_CONTENT
          , text $ getVariantRideType variant
          , margin (MarginHorizontal 7 2)
          , color Color.black800
          , singleLine true
          ]
        , imageView
          [ imageWithFallback $ case enabled of
                                  true  -> fetchImage FF_ASSET "ny_ic_check_mark"
                                  false -> fetchImage FF_COMMON_ASSET "ny_ic_cross_red"
          , width $ V 16
          , height $ V 16
          , gravity RIGHT
          , alpha if opacity then 0.5 else 1.0
          ]
       ]
   ]

toggleView :: forall w. (Action -> Effect Unit) -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
toggleView push enabled canDowngrade =
  let backgroundColor = if enabled then Color.blue800 else Color.black600
      align = if enabled then RIGHT else LEFT
  in  linearLayout
      [ width $ V 40
      , height $ V 22
      , cornerRadius 100.0
      , background backgroundColor
      , stroke $ "1," <> backgroundColor
      , gravity CENTER_VERTICAL
      , onClick push (const DowngradeVehicle)
      , clickable canDowngrade
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity align
          ][  linearLayout
              [ width $ V 16
              , height $ V 16
              , background Color.white900
              , cornerRadius 100.0
              , gravity CENTER_VERTICAL
              , margin (MarginHorizontal 2 2)
              ][]
          ]
      ]

defaultVehicleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
defaultVehicleView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , cornerRadius 8.0
    , padding $ Padding 16 20 16 30
    , margin $ Margin 16 16 16 16
    , stroke $ "1," <> Color.grey900
    ]
    [ vehicleDetailsView push state
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.grey700
      , margin $ MarginVertical 23 20
      ][]
    , vehicleLogoAndType push state
    ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleDetailsView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ orientation VERTICAL
        , weight 1.0
        ]
        [ customTV (getString YOUR_VEHICLE) FontSize.a_12 FontStyle.body3 Color.black650
        , customTV (capitalizeFirstChar state.data.vehicleName) FontSize.a_20 FontStyle.h3 Color.black800
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , cornerRadius 6.0
        , background Color.golden
        , padding $ Padding 3 3 3 3
        ]
        [ textView
            $ [ width MATCH_PARENT
              , height MATCH_PARENT
              , padding $ Padding 5 3 5 3
              , text state.data.vehicleNumber
              , color Color.black800
              , gravity CENTER
              , cornerRadius 3.0
              , stroke $ "2," <> Color.black800
              ] <> FontStyle.body8 TypoGraphy
        ]
    ]

vehicleLogoAndType :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleLogoAndType push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ imageView
            [ imageWithFallback $ getVehicleVariantImage state.data.vehicleType
            , gravity LEFT
            , height $ V 48
            , width $ V 48
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , margin $ MarginLeft 7
            ]
            [ customTV (getVariantRideType state.data.vehicleType) FontSize.a_20 FontStyle.h3 Color.black800
            , customTV (getVehicleCapacity state.data.vehicleType) FontSize.a_12 FontStyle.body3 Color.black650
            ]
        ]
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ PaddingVertical 10 10
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView $
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text $ getString BOOKING_OPTIONS
            , margin $ MarginLeft 20
            , color Color.black
            , weight 1.0
            , gravity CENTER_VERTICAL
            , alpha 0.8
            ] <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

downgradeOptionsView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
downgradeOptionsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 56
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingHorizontal 16 16
        , margin $ MarginBottom 14
        ]
        [ customTV (getString MAKE_YOURSELF_AVAILABLE_FOR) FontSize.a_20 FontStyle.h3 Color.black
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , padding $ PaddingVertical 6 6
                  ]
                  [ ChooseVehicle.view (push <<< ChooseVehicleAC) item
                  ]
            ) state.data.downgradeOptions
        )
    ]

customTV :: forall w. String -> Int -> (LazyCheck -> forall properties. (Array (Prop properties))) -> String -> PrestoDOM (Effect Unit) w
customTV text' textSize' fontStyle' color' =
  textView
    $ [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text text'
      , textSize textSize'
      , color color'
      ]
    <> fontStyle' TypoGraphy