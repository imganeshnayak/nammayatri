module Storage.Queries.Quote where

import App.Types
import Beckn.Storage.Redis.Queries
import Beckn.Types.Common (generateGUID)
import EulerHS.Prelude

createQuote :: Flow Text
createQuote = do
  quoteId <- generateGUID
  let ttl = 86400 -- 24hrs in secs
  setExRedis ("Quote_" <> quoteId) quoteId ttl
  return quoteId

lookupQuote :: Text -> Flow (Maybe Text)
lookupQuote key = do
  getKeyRedis ("Quote_" <> key)
