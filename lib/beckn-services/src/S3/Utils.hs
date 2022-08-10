module S3.Utils where

import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.MAC.HMAC (hmac)
import Data.ByteString as DB
import Data.ByteString.Base16 as Base16
import Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Prelude

formatISO8601 :: UTCTime -> String
formatISO8601 date =
  addZ $ T.unpack $ takeWhileDot $ replaceColon $ replaceDash $ T.pack $ iso8601Show date
  where
    replaceColon = T.replace (T.pack ":") space
    replaceDash = T.replace (T.pack "-") space
    takeWhileDot = T.takeWhile (/= '.')
    space = T.pack ""
    addZ val = val <> "Z"

hexSHA256 :: DB.ByteString -> DB.ByteString
hexSHA256 = Base16.encode . SHA256.hash

hmacSHA256_64 :: DB.ByteString -> DB.ByteString -> DB.ByteString
hmacSHA256_64 = hmac SHA256.hash 64

hex :: DB.ByteString -> DB.ByteString
hex = Base16.encode

s3AuthManagerKey :: String
s3AuthManagerKey = "s3-auth-signature"
