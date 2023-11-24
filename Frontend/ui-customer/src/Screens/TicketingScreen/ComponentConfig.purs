{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketingScreen.ComponentConfig where 

import Data.Maybe (Maybe(..))
import Screens.Types as ST
import MerchantConfig.DefaultConfig as DC
import Components.SettingSideBar.Controller as SideBar


settingSiderBarState :: ST.TicketingScreenState -> SideBar.SettingSideBarState
settingSiderBarState state = {
  opened : state.data.sideBarStatus
  , name : "String"
  , number : "String"
  , email : Nothing
  , gender : Nothing
  , appConfig : DC.config
  , sideBarList : ["MyRides", "Tickets", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "ShareApp", "LiveStatsDashboard", "About", "Logout"]
}