module Product.Quote where

import App.Types
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Types.API.Quote as API
import Types.Error
import qualified Types.ProductInfo as Info
import qualified Types.Storage.SearchRequest as SSR
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.SearchReqLocation as Location
import Utils.Common

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  fromLocation <- Location.findLocationById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- Location.findLocationById searchRequest.toLocationId >>= fromMaybeM LocationNotFound
  piList <- QPI.findAllByRequestId searchRequest.id
  quotes <- traverse buildQuote $ sortByNearestDriverDistance piList
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity toLocation,
        quotes
      }
  where
    sortByNearestDriverDistance piList = do
      let sortFunc = \(_, aDist) (_, bDist) -> do
            compare aDist bDist
      fmap fst $ sortBy sortFunc $ fmap (\prodInst -> (prodInst, fromMaybe (0 :: Double) $ readMaybe . T.unpack =<< prodInst.udf1)) piList
    buildQuote :: DBFlow m r => PI.ProductInstance -> m SQuote.QuoteAPIEntity
    buildQuote prodInst = do
      info :: Info.ProductInfo <- prodInst.info >>= decodeFromText & fromMaybeM (InternalError "Unable to read product info.")
      return $
        SQuote.QuoteAPIEntity
          { id = prodInst.id,
            estimatedPrice = fromMaybe 0 prodInst.price,
            agencyName = fromMaybe "" $ info.provider >>= (.name),
            agencyNumber = fromMaybe "" $ info.provider >>= (listToMaybe . (.phones)),
            agencyCompletedRidesCount = fromMaybe 0 $ info.provider >>= (.info) >>= (.completed),
            nearestDriverDistance = fromMaybe 0 $ readMaybe . T.unpack =<< prodInst.udf1,
            createdAt = prodInst.createdAt
          }
