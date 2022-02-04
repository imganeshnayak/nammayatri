module MockData.OnSearch where

import Beckn.Types.Amount
import Beckn.Types.Core.Migration.Gps
import Core.Common.Price
import Core.OnSearch
import Core.OnSearch.Departure
import Core.OnSearch.Descriptor
import Core.OnSearch.Fare
import Core.OnSearch.Item
import Core.OnSearch.LocationDetails
import Core.OnSearch.Provider
import Core.OnSearch.Route
import Data.Time.Clock
import Relude hiding (id, state)

mockBppDescriptor :: Descriptor
mockBppDescriptor =
  Descriptor
    { name = "State Water Transport Department",
      code = "SWTD",
      symbol = "SWTD bpp symbol",
      short_desc = "SWTD bpp description",
      long_desc = "SWTD bpp long description",
      images = []
    }

providerName :: Text
providerName = "State Water Transport Department"

providerDescriptorId :: DescriptorId
providerDescriptorId =
  DescriptorId
    { name = providerName
    }

onSearchCatalog :: UTCTime -> OnSearchCatalog
onSearchCatalog time =
  let bpp_descriptor = mockBppDescriptor
      bpp_providers = [mockProvider time]
      catalog = Catalog {..}
   in OnSearchCatalog {..}

mockProviderId :: Text
mockProviderId = "SWTD"

mockProvider :: UTCTime -> Provider
mockProvider time =
  let id = mockProviderId
      descriptor = providerDescriptorId
      locations = mockLocations
      routes = mockRoutes
      fares = mockFares
      departures = mockDepartures time
      items = mockItems
   in Provider {..}

locationLabelEKM, locationLabelEMB, locationLabelABC :: Text
locationLabelEKM = "EKM"
locationLabelEMB = "EMB"
locationLabelABC = "ABC"

mockLocations :: [LocationDetails]
mockLocations = [locationEKM, locationEMB, locationABC]

buildLocation :: Text -> Text -> Gps -> LocationDetails
buildLocation id name gps =
  let stop_code = id
      descriptor = DescriptorId {..}
   in LocationDetails {..}

exampleGps :: Gps
exampleGps =
  Gps
    { lat = 9.898,
      lon = 76.324
    }

locationGpsEKM, locationGpsEMB, locationGpsABC :: Gps
locationGpsEKM = exampleGps
locationGpsEMB = exampleGps {lat = exampleGps.lat + 1}
locationGpsABC = exampleGps {lat = exampleGps.lat + 1, lon = exampleGps.lon + 0.2}

locationEKM, locationEMB, locationABC :: LocationDetails
locationEKM = buildLocation locationLabelEKM "Ernakulam" locationGpsEKM
locationEMB = buildLocation locationLabelEMB "Embarkment" locationGpsEMB
locationABC = buildLocation locationLabelABC "Test Station" locationGpsABC

mockRoutes :: [Route]
mockRoutes = [routeEkmAbc, routeEkmEmb]

routeEkmAbc, routeEkmEmb :: Route
routeEkmAbc = buildRoute routeEkmAbcCode routeEkmAbcId locationLabelEKM locationLabelABC
routeEkmEmb = buildRoute routeEkmEmbCode routeEkmEmbId locationLabelEKM locationLabelEMB

routeEkmAbcId, routeEkmEmbId :: Text
routeEkmAbcId = "EKM-ABC"
routeEkmEmbId = "EKM-EMB"

routeEkmAbcCode, routeEkmEmbCode :: Text
routeEkmAbcCode = "AAA-DDD"
routeEkmEmbCode = "BBB-CCC"

buildRoute :: Text -> Text -> Text -> Text -> Route
buildRoute code id start end = do
  let start_stop = start
      end_stop = end
      route_code = code
  Route {..}

mockFares :: [Fare]
mockFares = [fareEkmAbc1, fareEkmEmb2]

fareEkmAbc1, fareEkmEmb2 :: Fare
fareEkmAbc1 = buildFare "1" routeEkmAbcId 60
fareEkmEmb2 = buildFare "2" routeEkmEmbId 30

buildFare :: Text -> Text -> Amount -> Fare
buildFare id route_id amount = do
  let price = Price "INR" amount
  Fare {..}

mockDepartures :: UTCTime -> [Departure]
mockDepartures time = map ($ time) [departureEkmAbc1, departureEkmEmb2]

buildDeparture :: Text -> Text -> UTCTime -> Departure
buildDeparture id route_id start = do
  let start_time = TimeStamp start
      hour = 60 * 60
      end_time = TimeStamp $ addUTCTime hour start
  Departure {..}

departureEkmAbc1 :: UTCTime -> Departure
departureEkmAbc1 = buildDeparture "1" routeEkmAbcId

departureEkmEmb2 :: UTCTime -> Departure
departureEkmEmb2 = buildDeparture "2" routeEkmEmbId

mockItems :: [Item]
mockItems = [itemEkmAbc1, itemEkmEmb2]

itemEkmAbc1, itemEkmEmb2 :: Item
itemEkmAbc1 = Item "1" "1" True
itemEkmEmb2 = Item "2" "2" True
