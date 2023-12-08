{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ApplicationStatusScreen.Handler where
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.ApplicationStatusScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Screens.ApplicationStatusScreen.View as ApplicationStatusScreen
import Types.App (FlowBT, GlobalState(..), APPLICATION_STATUS_SCREENOUTPUT(..),ScreenType(..))
import React.Navigation.Navigate (navigateToScreen)


applicationStatus :: String -> FlowBT String APPLICATION_STATUS_SCREENOUTPUT
applicationStatus screenType = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ ApplicationStatusScreen.screen state.applicationStatusScreen screenType
  case action of
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_HOME_FROM_APPLICATION_STATUS
    GoToDlScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_UPLOAD_DL_SCREEN
    GoToVehicleDetailScreen ->  App.BackT $ App.BackPoint <$> pure GO_TO_VEHICLE_DETAIL_SCREEN
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_ACCOUT
    GoToEnterOtp state -> App.BackT $ App.BackPoint <$> (pure $ VALIDATE_NUMBER state)
    AddMobileNumber state -> App.BackT $ App.BackPoint <$> (pure $ VALIDATE_OTP state) 
    ResendOtp state-> App.BackT $ App.BackPoint <$> (pure $ RESEND_OTP_TO_ALTERNATE_NUMBER state)