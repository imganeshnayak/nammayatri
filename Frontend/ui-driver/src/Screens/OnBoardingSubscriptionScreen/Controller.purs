module Screens.OnBoardingSubscriptionScreen.Controller where

import Prelude

import Components.PrimaryButton as PrimaryButton
import Data.Array ((!!))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Debug (spy)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge as JB
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (OnBoardingSubscriptionScreenState)
import Screens.Types (PlanCardConfig)
import Services.API (UiPlansResp(..))
import Storage (KeyStore(..), setValueToLocalStore)
import Components.PopUpModal as PopUpModal


instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        BackPressed -> do
                trackAppBackPress appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        NoAction -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "no_action"
        GoToRegisteration -> do
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        CallSupport -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "call_support"
        SelectPlan config -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "select_plan"
        JoinPlanAC act -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "join_plan"
        _ -> pure unit

data ScreenOutput =  GoBack | GoToRegisterationScreen OnBoardingSubscriptionScreenState | StartFreeTrialExit OnBoardingSubscriptionScreenState

data Action = BackPressed
            | NoAction
            | GoToRegisteration
            | LoadPlans UiPlansResp
            | SelectPlan PlanCardConfig
            | JoinPlanAC PrimaryButton.Action
            | CallSupport
            | PopUpModalAC PopUpModal.Action

eval :: Action -> OnBoardingSubscriptionScreenState -> Eval Action ScreenOutput OnBoardingSubscriptionScreenState
eval BackPressed state = 
    if state.props.supportPopup then exit GoBack
    else continue state {props {supportPopup = not state.props.supportPopup}}
    
eval NoAction state = continue state
eval GoToRegisteration state = exit $ GoToRegisterationScreen state
eval (LoadPlans plans) state = do
    let (UiPlansResp planResp) = plans
        config = state.data.subscriptionConfig
    _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
    let planList = planListTransformer plans false config.gradientConfig 
    continue state { data{ plansList = planList , selectedPlanItem = (planList !! 0)}}
eval (SelectPlan config ) state = continue state {data { selectedPlanItem = Just config }}
eval (JoinPlanAC PrimaryButton.OnClick) state = updateAndExit state $ StartFreeTrialExit state
eval CallSupport state = do
  _ <- pure $ JB.showDialer state.data.subscriptionConfig.supportNumber false
  continue state
eval (PopUpModalAC PopUpModal.OnButton1Click) state = do
    void $ pure $ JB.showDialer state.data.subscriptionConfig.supportNumber false
    continue state { props { supportPopup = false }}
eval (PopUpModalAC (PopUpModal.OnButton2Click)) _ = exit GoBack
eval _ state = continue state
