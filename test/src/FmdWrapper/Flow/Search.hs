module FmdWrapper.Flow.Search where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Example
import Common
import Control.Lens ((?~))
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import ExternalAPI.Dunzo.Types
import Fmd
import FmdWrapper.Common
import qualified FmdWrapper.Fixtures as Fixtures
import FmdWrapper.Server
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, type (:>))
import Servant.Client
import Test.Hspec hiding (context, example)
import qualified "fmd-wrapper" Types.Beckn.API.Search as SearchAPI
import "fmd-wrapper" Types.Beckn.Category (Category (..))
import "fmd-wrapper" Types.Beckn.Context (Action (..), Context (..))
import qualified "fmd-wrapper" Types.Beckn.Descriptor as Descriptor
import qualified "fmd-wrapper" Types.Beckn.Domain as Domain
import qualified "fmd-wrapper" Types.Beckn.Error as Error
import "fmd-wrapper" Types.Beckn.Gps (Gps (..))
import Utils

numberOfDunzoCategores :: Int
numberOfDunzoCategores = 6

runSearch :: ClientEnv -> Text -> BecknReq SearchAPI.SearchIntent -> IO (Either ClientError AckResponse)
runSearch clientEnv orgId searchReq = do
  now <- getPOSIXTime
  let signature = decodeUtf8 $ signRequest searchReq now orgId (orgId <> "-key")
  let searchAPI = Proxy :: Proxy (Header "Authorization" Text :> SearchAPI.SearchAPI)
  runClient clientEnv $ client searchAPI (Just signature) searchReq

verifyCallbackContext :: Bool -> Text -> Context -> IO ()
verifyCallbackContext expectBppUri transactionId context = do
  context.country `shouldBe` "IND"
  context.domain `shouldBe` Domain.FINAL_MILE_DELIVERY
  when expectBppUri $ context.bpp_uri `shouldSatisfy` isJust
  context.transaction_id `shouldBe` transactionId
  context.action `shouldBe` ON_SEARCH
  context.message_id `shouldBe` transactionId
  context.core_version `shouldBe` "0.9.1"

verifyDunzoCatalog :: SearchAPI.OnSearchCatalog -> IO ()
verifyDunzoCatalog onSearchCatalog = do
  let catalog = SearchAPI.catalog onSearchCatalog
  let Just [provider] = catalog.bpp_providers
  let items_ = fromMaybe [] provider.items
  let categories = fromMaybe [] provider.categories
  case categories of
    [category] ->
      category
        `shouldBe` Category
          (Just "1")
          Nothing
          ( Just
              Descriptor.emptyDescriptor
                { Descriptor.name = Just "Pickup and drop",
                  Descriptor.code = Just "pickup_drop"
                }
          )
          Nothing
          Nothing
    _ -> expectationFailure "Exactly one category expected."
  map extractCategoryData items_ `shouldBe` expectedItemsCategoryData
  forM_ items_ $ \item -> do
    let price_ = fromJust item.price
    price_.currency `shouldBe` Just "INR"
    price_.estimated_value `shouldSatisfy` isJust
    item.category_id `shouldBe` Just "1"
  where
    extractCategoryData item =
      let descriptor = fromJust (item.descriptor)
       in (fromJust item.id, descriptor.name, descriptor.code)
    expectedItemsCategoryData =
      let catData = map (Just . content) dzPackageContentList
       in zip3 stringNumbers catData catData
    stringNumbers = map show numbers
    numbers = [1 ..] :: [Int]

processResults :: Text -> CallbackData -> IO [BecknCallbackReq SearchAPI.OnSearchCatalog]
processResults transactionId callbackData = do
  callbackResults <- readMVar (onSearchCb callbackData)
  let apiKeys = map apiKey callbackResults
  let searchResults = map result callbackResults

  traverse_ verifyApiKey apiKeys
  verifyContexts searchResults

  return searchResults
  where
    verifyContexts searchResults =
      forM_ searchResults \searchResult ->
        verifyCallbackContext True transactionId $ searchResult.context

dunzoLocationError ::
  Gps ->
  Gps ->
  (Error.Error -> Expectation) ->
  ClientEnv ->
  CallbackData ->
  IO ()
dunzoLocationError pickupGps dropGps check clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext SEARCH transactionId

    let searchReq =
          buildFMDSearchReq ctx
            & setIntentPickupGps ?~ pickupGps
            & setIntentDropGps ?~ dropGps

    gatewayResp <- runSearch clientEnv "fmd-test-app" searchReq
    assertAck gatewayResp

    waitForCallback
    searchResults <- processResults transactionId callbackData

    let errorResults = filter isLeft $ map contents searchResults
    case errorResults of
      [Left err] -> check err
      _ -> expectationFailure "Exactly one error result expected."

successfulSearch :: ClientEnv -> CallbackData -> IO ()
successfulSearch clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext SEARCH transactionId

    let searchReq =
          buildFMDSearchReq ctx
            & setIntentPickupGps ?~ Fixtures.validDunzoGps1
            & setIntentDropGps ?~ Fixtures.validDunzoGps2

    gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
    assertAck gatewayResponse

    waitForCallback
    searchResults <- processResults transactionId callbackData

    let dunzoResults = filter isDunzoResult searchResults

    case rights (map contents dunzoResults) of
      [message] ->
        verifyDunzoCatalog message
      _ -> expectationFailure "Expected one search result from Dunzo."
  where
    isDunzoResult result =
      result.context.bpp_uri == Just fmdWrapperBaseUrl

dunzoUnserviceableLocation :: ClientEnv -> CallbackData -> IO ()
dunzoUnserviceableLocation =
  dunzoLocationError
    Fixtures.validDunzoGps1
    example
    ( const (return ())
    )

dunzoNearByLocation :: ClientEnv -> CallbackData -> IO ()
dunzoNearByLocation =
  dunzoLocationError
    Fixtures.validDunzoGps1
    Fixtures.validDunzoGps1
    ( const (return ())
    )

dunzoDifferentCity :: ClientEnv -> CallbackData -> IO ()
dunzoDifferentCity =
  dunzoLocationError
    Fixtures.validDunzoGps1
    Fixtures.differentCity
    ( \Error.Error {..} -> do
        code `shouldBe` "FMD001"
        _type `shouldBe` Error.DOMAIN_ERROR
    )

unknownSubscriber :: ClientEnv -> CallbackData -> IO ()
unknownSubscriber clientEnv _ = do
  ctx <- buildContext SEARCH "dummy-txn-id"
  let searchReq = buildFMDSearchReq ctx

  gatewayResponse <- runSearch clientEnv "" searchReq
  verifyError 401 "SIGNATURE_VERIFICATION_FAILURE" gatewayResponse

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    describe "Search API" do
      it "Successful search" $ successfulSearch appClientEnv
      it "Dunzo: unserviceable location" $ dunzoUnserviceableLocation appClientEnv
      it "Dunzo: nearby location" $ dunzoNearByLocation appClientEnv
      it "Dunzo: different city" $ dunzoDifferentCity appClientEnv
      it "Unknown subscriber" $ unknownSubscriber appClientEnv