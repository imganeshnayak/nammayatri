module Lib.UtilsTh where

import Data.Bool
import Data.Text
import Prelude

data UrlParts
  = UnitPath Text
  | Capture Text Text -- Capture Name Type
  | QueryParam Text Text Bool
  deriving (Show) -- QueryParam Name Type Mandatory
