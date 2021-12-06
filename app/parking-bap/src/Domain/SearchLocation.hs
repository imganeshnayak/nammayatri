{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.SearchLocation where

import Beckn.Prelude
import Beckn.Types.Id

data SearchLocation = SearchLocation
  { id :: Id SearchLocation,
    lat :: Double,
    lon :: Double,
    createdAt :: UTCTime
  }
