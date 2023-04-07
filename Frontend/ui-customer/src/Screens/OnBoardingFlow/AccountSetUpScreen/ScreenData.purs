{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.ScreenData where

import Screens.Types (AccountSetUpScreenState)
import Data.Maybe (Maybe(..))

initData :: AccountSetUpScreenState
initData =
  { data:
      { name: ""
      , email: ""
      , gender: Nothing
      }
  , props:
      { btnActive: false
      , backPressed: false
      , genderSelected: Nothing
      , genderOptionExpanded: false
      , expandEnabled : false
      }
  }
