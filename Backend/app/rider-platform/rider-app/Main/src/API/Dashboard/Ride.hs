{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified Domain.Action.Dashboard.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Types.Merchant as DM

type API =
  "ride"
    :> ( ShareRideInfoAPI
           :<|> Common.RideListAPI
       )

type ShareRideInfoAPI = Common.ShareRideInfoAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  shareRideInfo merchantId
    :<|> rideList merchantId

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId reqRideId = withFlowHandlerAPI $ DRide.shareRideInfo merchantShortId reqRideId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone =
  withFlowHandlerAPI $ DRide.rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone
