module Screens.WelcomeScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, accessibility, afterRender, background, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, orientation, padding, weight, width)
import Screens.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (WelcomeScreenState)
import JBridge (addCarousel)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Function.Uncurried (runFn2)
import Screens.WelcomeScreen.ComponentConfig
import Data.Maybe (Maybe(..))

screen :: WelcomeScreenState -> ScopedScreen Action WelcomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Nothing
  , name: "WelcomeScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "WelcomeScreen ----- state" state
          let _ = spy "WelcomeScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> WelcomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility DISABLE
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background "#FFFAED"
        , padding $ PaddingBottom 24
        ][  imageView
            [ height $ V 50
            , width $ V 147
            , margin $ MarginTop 50
            , imageWithFallback "ic_namma_yatri_logo,https://assets.juspay.in/nammayatri/images/user/ic_namma_yatri_logo.png"   -- "ic_namma_yatri_logo"
            ]
            , carouselView state push
            , PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)
        ]


carouselView:: WelcomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "CarouselView"
    , accessibility DISABLE
    , gravity CENTER
    , weight 1.0
    , margin $ MarginBottom 20
    , afterRender (\action -> do
        _ <- push action
        _ <- runFn2 addCarousel (carouselData state) (getNewIDWithTag "CarouselView")
        pure unit
        ) (const AfterRender)
    ][]