{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module LoaderOverlay.Controller where

import Prelude

import Prelude (class Show, discard, pure, unit)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import PrestoDOM (Eval, exit, continue, continueWithCmd)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog
data Action
  = AfterRender

data ScreenOutput
  = Exit

eval :: forall a. Action -> a -> Eval Action ScreenOutput a
eval AfterRender state = exit Exit
