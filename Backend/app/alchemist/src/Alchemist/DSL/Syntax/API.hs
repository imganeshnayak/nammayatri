{-# LANGUAGE TemplateHaskell #-}

module Alchemist.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Kernel.Prelude

-- import Data.Map (Map)

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth deriving (Show)

data HeaderType = Header Text Text deriving (Show)

data ApiReq = ApiReq Text Text deriving (Show)

data ApiRes = ApiRes Text Text deriving (Show)

data Apis = Apis
  { _apis :: [ApiTT],
    _moduleName :: Text
  }
  deriving (Show)

data ApiTT = ApiTT
  { _urlParts :: [UrlParts],
    _apiType :: ApiType,
    _authType :: Maybe AuthType,
    _header :: [HeaderType],
    _apiReqType :: Maybe ApiReq,
    _apiResType :: ApiRes
  }
  deriving (Show)

$(makeLenses ''ApiTT)

data ApiParts = ApiTU ApiType [UrlParts] | HeaderT HeaderType | Auth (Maybe AuthType) | Req Text Text | Res Text Text | ModuleName Text deriving (Show)

-- data YamlObj = YamlObj {
--   _yamlModuleName :: String,
--   _yamlApi :: [(String, Map String String)]
-- } deriving (Show)

-- data YamlApi = YamlApi {

-- } deriving (Show)
