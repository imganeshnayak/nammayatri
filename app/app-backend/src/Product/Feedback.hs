module Product.Feedback where

import qualified App.Types as App
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.Migration1.API.Types as Common
import qualified Beckn.Types.Core.Migration1.Rating as Rating
import Beckn.Types.Id
import Beckn.Utils.Logging
import EulerHS.Prelude hiding (product)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Feedback as API
import Types.Error
import qualified Types.Storage.Person as Person
import Utils.Common

feedback :: Id Person.Person -> API.FeedbackReq -> App.FlowHandler API.FeedbackRes
feedback personId request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  let txnId = getId rideBooking.requestId
  let quoteId = getId rideBooking.quoteId
  organization <-
    Organization.findOrganizationById (rideBooking.providerId)
      >>= fromMaybeM OrgNotFound
  bapURIs <- asks (.bapSelfURIs)
  bppURI <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  context <- buildMobilityContext1 txnId bapURIs.cabs (Just bppURI)
  ExternalAPI.feedback bppURI (Common.BecknReq context (Rating.RatingMessage quoteId ratingValue))
  return Success
