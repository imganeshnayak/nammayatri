{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.View where

import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, width, cornerRadius, weight, afterRender, imageWithFallback,onBackPressed,background)
import Effect (Effect)
import Language.Strings(getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Components.PrimaryButton as PrimaryButton
import Screens.PermissionsScreen.Controller (Action(..), eval, ScreenOutput, getTitle, getDescription)
import Screens.Types as ST
import Screens.PermissionsScreen.ScreenData (Permissions(..), permissionsList, Listtype)
import JBridge as JB
import Common.Types.App
import Screens.PermissionsScreen.ComponentConfig
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Components.PopUpModal as PopUpModal
import Components.StepsHeaderModel as StepsHeaderModel

screen :: ST.PermissionsScreenState -> Screen Action ST.PermissionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "PermissionsScreen"
  , globalEvents : [ (\ push -> do
    _ <- JB.storeCallBackDriverLocationPermission push LocationPermissionCallBack
    _ <- JB.storeCallBackOverlayPermission push OverlayPermissionSwitchCallBack
    _ <- JB.storeCallBackBatteryUsagePermission push BatteryUsagePermissionCallBack
    pure $ pure unit)]
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const AfterRender)
    , onBackPressed push (const BackPressed)
    ]([  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility if state.props.logoutModalView then GONE else VISIBLE
        , weight 1.0
        ][ scrollView
            [ width MATCH_PARENT
            , weight 1.0
            , height MATCH_PARENT
            ][ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ][ PrestoAnim.animationSet
                 [ Anim.fadeIn true
                 ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderModelConfig state) 
                 , permissionsListView state push
                ]
               ]
            ]
        ,  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility if state.props.logoutModalView then GONE else VISIBLE
            ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
      ]<> if state.props.logoutModalView then [logoutPopupModal push state] else [])


headerLayout :: forall w. ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , margin (MarginTop 50)
 ][ textView (
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_HORIZONTAL
    , margin (MarginTop 20)
    , text (getString WE_NEED_SOME_ACCESS)
    , color Color.black800
    ] <> FontStyle.h1 TypoGraphy
    )
 ]


permissionsListView :: forall w. ST.PermissionsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
permissionsListView state push = 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , padding (PaddingVertical 10 5)
    ][linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ] (map(\item ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin (Margin 15 25 15 0)
            , padding (PaddingVertical 15 15)
            , onClick push (const (ItemClick item.permission))
            , stroke ("1," <> ( case item.permission of
                                  Location -> if state.props.isLocationPermissionChecked   then  Color.green900 else Color.grey900
                                  Overlay -> if state.props.isOverlayPermissionChecked     then  Color.green900 else Color.grey900
                                  AutoStart -> if state.props.isAutoStartPermissionChecked then  Color.green900 else Color.grey900
                                  Battery -> if state.props.isBatteryOptimizationChecked   then  Color.green900 else Color.grey900 ))
            , cornerRadius 6.0 
            , background ( case item.permission of
                                  Location -> if state.props.isLocationPermissionChecked   then  Color.green200 else Color.white900
                                  Overlay -> if state.props.isOverlayPermissionChecked     then  Color.green200 else Color.white900
                                  AutoStart -> if state.props.isAutoStartPermissionChecked then  Color.green200 else Color.white900
                                  Battery -> if state.props.isBatteryOptimizationChecked   then  Color.green200 else Color.white900 )
            ][  titleImage item,
                titleAndDescriptionList item,
                checkBox item state
            ]) permissionsList)
    ]

titleAndDescriptionList :: forall w. Listtype -> PrestoDOM (Effect Unit) w
titleAndDescriptionList item = 
 linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , weight 1.0
    ][  textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getTitle item.permission)
        , color Color.black800
        , margin (MarginBottom 8)
        ] <> FontStyle.subHeading1 TypoGraphy
        ),
        textView (
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text (getDescription item.permission)
        , color Color.black700
        , margin (MarginRight 40)
        ] <> FontStyle.paragraphText TypoGraphy
        )
    ]

checkBox :: forall w. Listtype -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
checkBox item state = 
 linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity RIGHT
    , padding (Padding 0 22 20 0)
    , layoutGravity "right"
    ][ frameLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT      
    ][ linearLayout
        [ height (V 18)
        , width (V 18)
        , stroke ("1," <> Color.black)
        , cornerRadius 9.0
        ][]
        , imageView
        [ width (V 18)
        , height (V 18)
        , imageWithFallback "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
        , visibility case item.permission of
            Location -> if state.props.isLocationPermissionChecked then VISIBLE else GONE
            Overlay -> if state.props.isOverlayPermissionChecked then VISIBLE else GONE
            AutoStart -> if state.props.isAutoStartPermissionChecked then VISIBLE else GONE
            Battery -> if state.props.isBatteryOptimizationChecked then VISIBLE else GONE
        ]
      ]
    ]

titleImage :: forall w. Listtype -> PrestoDOM (Effect Unit) w
titleImage item = 
 imageView
    [ imageWithFallback case item.permission of
     Location -> "ny_ic_permission_location,https://assets.juspay.in/nammayatri/images/driver/ny_ic_permission_location.png"
     Overlay -> "ny_ic_permission_overlay,https://assets.juspay.in/nammayatri/images/driver/ny_ic_permission_overlay.png"
     AutoStart -> "ny_ic_permission_autostart,https://assets.juspay.in/nammayatri/images/driver/ny_ic_permission_autostart.png"
     Battery -> "ny_ic_permission_battery,https://assets.juspay.in/nammayatri/images/driver/ny_ic_permission_battery.png"
    , width (V 44)
    , height (V 44)
    , margin (Margin 15 8 15 0)
    ]
  

logoutPopupModal :: forall w . (Action -> Effect Unit) -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
logoutPopupModal push state =
       linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.blackLessTrans
        ][ PopUpModal.view (push <<< PopUpModalLogoutAction) (logoutPopUp state) ] 