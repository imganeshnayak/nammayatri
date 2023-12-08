{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.Handler where


import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.UploadDrivingLicenseScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.UploadDrivingLicenseScreen.View as UploadDrivingLicenseScreen
import Types.App (FlowBT, GlobalState(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)


uploadDrivingLicense :: FlowBT String UPLOAD_DRIVER_LICENSE_SCREENOUTPUT
uploadDrivingLicense = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ UploadDrivingLicenseScreen.screen state.uploadDrivingLicenseScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> updatedState
      modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = false}, data {errorMessage = ""}}
      App.BackT $ App.NoBack <$> (pure $ GOTO_ONBOARDING_FLOW)
    ValidateDetails updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ VALIDATE_DL_DETAILS updatedState)
    ValidateDataCall updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ VALIDATE_DATA_API updatedState)
    AddVehicleDetailsScreen -> App.BackT $ App.BackPoint <$> (pure $ GOTO_VEHICLE_DETAILS_SCREEN)
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_ACCOUNT
    GoToRegisteration -> App.BackT $ App.BackPoint <$> pure GOTO_ONBOARDING_FLOW