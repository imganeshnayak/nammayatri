module Types.API.ProductInstance where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import Beckn.Types.Storage.ProductInstance
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.JSON
import Data.Swagger
import EulerHS.Prelude

data ProdInstReq = ProdInstReq
  { status :: [ProductInstance.ProductInstanceStatus],
    limit :: Integer,
    offset :: Integer
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdInstReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProdInstReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ProductInstanceRes = ProductInstanceRes
  { _case :: Case.Case,
    product :: Product.Products,
    productInstance :: ProductInstance,
    fromLocation :: Maybe Loc.Location,
    toLocation :: Maybe Loc.Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProductInstanceRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProductInstanceRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type ProductInstanceList = [ProductInstanceRes]
