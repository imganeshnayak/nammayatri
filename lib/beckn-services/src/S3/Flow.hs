module S3.Flow where

import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Utils.Common
import Data.Text as T
import EulerHS.Prelude hiding (decodeUtf8, show, traceShowId)
import qualified EulerHS.Types as ET
import S3.Error
import S3.Types
import S3.Utils
import Servant
import Servant.Client

type S3GetAPI = Get '[S3ImageData] Text

type S3PutAPI =
  ReqBody '[S3ImageData] Text
    :> Put '[S3OctetStream] Text

s3GetAPI :: Proxy S3GetAPI
s3GetAPI = Proxy

s3PutAPI :: Proxy S3PutAPI
s3PutAPI = Proxy

url :: String -> String -> BaseUrl
url path host =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = host,
      baseUrlPort = 443,
      baseUrlPath = path
    }

s3Host :: Text -> String
s3Host bN = T.unpack bN <> ".s3.amazonaws.com"

get ::
  ( CoreMetrics m,
    HasFlowEnv m r '["s3Config" ::: S3Config]
  ) =>
  String ->
  m Text
get path = do
  withLogTag "S3" $ do
    S3Config {..} <- asks (.s3Config)
    let host = s3Host bucketName
    callS3API
      (url path host)
      (ET.client s3GetAPI)
      "GetS3"

put ::
  ( CoreMetrics m,
    HasFlowEnv m r '["s3Config" ::: S3Config]
  ) =>
  String ->
  Text ->
  m Text
put path img = do
  withLogTag "S3" $ do
    S3Config {..} <- asks (.s3Config)
    let host = s3Host bucketName
    callS3API
      (url path host)
      (ET.client s3PutAPI img)
      "PutS3"

callS3API :: CallAPI env a
callS3API =
  callApiUnwrappingApiError
    (identity @S3Error)
    (Just s3AuthManagerKey)
    (Just "S3_NOT_AVAILABLE")
