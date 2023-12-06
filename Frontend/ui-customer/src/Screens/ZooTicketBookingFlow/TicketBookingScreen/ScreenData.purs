{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingScreen.ScreenData where

import Common.Types.App as Common
import Data.Maybe (Maybe(..))
import MerchantConfig.DefaultConfig as DC
import Screens.Types (TicketBookingScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..), TicketBookingPeopleCategoryDetails(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..))
import Services.API (PlaceType(..), BookingStatus(..))

initData :: TicketBookingScreenState
initData = 
  { data : {
      placeInfo : Nothing,
      servicesInfo : [],
      servicesAvailing : [],
      shortOrderId: "",
      dateOfVisit : "",
      keyValArray : [],
      bookedForArray : [],
      zooName : "",
      transactionId : "",
      totalAmount : 0,
      selectedPlaceType : Other
  }
  , props : {
      currentStage : DescriptionStage,
      termsAndConditionsSelected : true,
      validDate : true,
      showShimmer : true,
      paymentStatus : Common.Scheduled,
      previousStage : DescriptionStage,
      ticketBookingList : dummyData,
      selectedBookingId : "",
      selectedBookingInfo : {
          shortId : "String",
          ticketPlaceId : "ticketPlcaId",
          ticketPlaceName : "ticketPlaceName",
          personId : "personId",
          amount : 66.0,
          visitDate : "2023-11-30",
          status : Pending,
          services : [dummyServiceDetails, dummyServiceDetails2]
      },
      activeListItem : dummyServiceDetails,
      activeIndex : 0,
      rightButtonDisable : false,
      leftButtonDisable : true,
      navigateToHome : true,
      selectedOperationalDay : ""
    }
  }

dummyData :: TicketBookings
dummyData = { booked : [{shortId : "kjdfk;a", ticketPlaceName : "Zoological Garden, AliPore", amount : 500.0, visitDate : "10-10-2023", status : Booked, ticketPlaceId : "", personId : "" }],
              pendingBooking : [{shortId : "kjdfadfk;a", ticketPlaceName : "Zoological Garden, AliPore", amount : 500.0, visitDate : "10-10-2023", status : Pending, ticketPlaceId : "", personId : ""}]
            }


dummyServiceDetails :: TicketBookingServiceDetails
dummyServiceDetails =
  { amount : 55.0,
    status : "dkfj;a",
    verificationCount : 5,
    expiryDate : Just "akfjk;asd",
    ticketServiceName : "dkj;akl",
    categories : [dummyCategory, dummyCategory],
    ticketServiceShortId : "dkajf;a",
    slot : Just "13:00:00"
  }

dummyServiceDetails2 :: TicketBookingServiceDetails
dummyServiceDetails2 =
  { amount : 55.0,
    status : "dkfj;a",
    verificationCount : 5,
    expiryDate : Just "akfjk;asd",
    ticketServiceName : "dkj;ak asdfasfd  afasd l",
    categories : [dummyCategory, dummyCategory],
    ticketServiceShortId : "dkajf;a asdfasd ",
    slot : Just "13:00:00"
  }

dummyCategory :: TicketBookingCategoryDetails
dummyCategory =
  { amount : 53.0,
    bookedSeats : 5,
    name : "catName",
    peopleCategories : [pc, pc]
  }

pc :: TicketBookingPeopleCategoryDetails
pc =
 {  name : "Adult",
    numberOfUnits : 5,
    pricePerUnit : 55.0
 }