{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Alchemist.DSL.Parser.API where

import Alchemist.DSL.Syntax.API
import Control.Lens hiding (noneOf)
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap (toList)
import Data.Bool
import qualified Data.Foldable as DT
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Yaml ((.:), Value, Object)
import qualified Data.Yaml as DA
import Text.Parsec
import Text.Parsec.Text
import Prelude

-- import qualified Data.Vector as V
-- import Data.Map (Map)

parseValue :: DA.Value -> Maybe Object
parseValue (DA.Object apiObj) = Just apiObj
parseValue _ = Nothing

parseAll :: DA.Value -> [ApiParts]
parseAll obj = concatMap (\x -> parseModuleName x) (mkList obj)

parseModuleName :: (Text, DA.Value) -> [ApiParts]
parseModuleName obj = case obj of
  ("Module", String moduleObj) -> [ModuleName moduleObj]
  ("API", Array apiObj) -> concatMap (apiParseYaml <$> mkList) (DT.toList apiObj)
  _ -> [ModuleName "Module Name Not Found"]

apiParseYaml :: [(Text, DA.Value)] -> [ApiParts]
apiParseYaml [(method, methodObj)] = map (\x -> apiTParserYaml method x) (mkList methodObj)
apiParseYaml _ = [ModuleName "Outside Case"]

apiTParserYaml :: Text -> (Text, DA.Value) -> ApiParts
apiTParserYaml methodName ("endpoint", String urlVal) = endPointParser methodName urlVal
apiTParserYaml _ ("auth", String authValue) = case authValue of
  "AdminTokenAuth" -> Auth (Just AdminTokenAuth)
  _ -> Auth (Just TokenAuth)
apiTParserYaml _ ("request", String reqTypeVal) = Req "JSON" reqTypeVal
apiTParserYaml _ ("response", String resTypeVal) = Res "JSON" resTypeVal
apiTParserYaml _ _ = ModuleName "Case not Working"

endPointParser :: Text -> Text -> ApiParts
endPointParser method urlVal =
  let apiTyp = apiTypeParserYaml method
   in case (urlParser urlVal) of
        Left _ -> ModuleName "Url Parsing Error"
        Right urlParsed -> (ApiTU apiTyp urlParsed)

apiTypeParserYaml :: Text -> ApiType
apiTypeParserYaml method = case method of
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE
  _ -> GET

mkList :: DA.Value -> [(Text, DA.Value)]
mkList (Object obj) = toList obj >>= \(k, v) -> [(toText k, v)]
mkList _ = []

apiParser :: String -> Either ParseError Apis
apiParser input = do
  let singleApis = filter (/= T.empty) $ T.splitOn "---" (T.pack input)
  mapM singleApiParser singleApis

singleApiParser :: Text -> Either ParseError ApiTT -- Either ParseError ApiTT
singleApiParser input = do
  api <- mapM (parse lineParserAPI "" . T.strip) (filter (/= T.empty) $ T.split (== '\n') (T.dropAround (\x -> x == ' ' || x == '\n') input))
  return $
    foldl
      ( \acc z ->
          case z of
            ModuleName y -> acc & moduleName .~ y
            HeaderT y -> over header (\x -> x <> [y]) acc
            ApiTU apitype urlparts -> acc & (apiType .~ apitype) . (urlParts .~ urlparts)
            Auth authtype -> acc & authType .~ authtype
            Req name tp -> acc & apiReqType ?~ ApiReq name tp
            Res name tp -> acc & apiResType .~ ApiRes name tp
      )
      (ApiTT {_apiReqType = Nothing, _authType = Nothing, _header = []})
      api

lineParserAPI :: Parser ApiParts
lineParserAPI = try moduleNameParser <|> try apiTypeAndURLParser <|> try authParser <|> try headerParser <|> try reqParser <|> resParser

moduleNameParser :: Parser ApiParts
moduleNameParser = do
  _ <- string "Module"
  spaces
  modelName <- T.pack <$> many1 (noneOf "")
  return $ ModuleName modelName

apiTypeAndURLParser :: Parser ApiParts
apiTypeAndURLParser = do
  apiTyp <- apiTypeParser
  spaces
  rawUrl <- T.pack <$> many1 (noneOf "")
  url <- either (const (fail "Url Parsing Error")) pure $ urlParser rawUrl
  return $ ApiTU apiTyp url

apiTypeParser :: Parser ApiType
apiTypeParser =
  choice
    [ GET <$ string "GET",
      POST <$ string "POST",
      PUT <$ string "PUT",
      DELETE <$ string "DELETE"
    ]

urlParser :: Text -> Either ParseError [UrlParts]
urlParser input = do
  let (url, queryparam) = T.breakOn "?" input
  (<>) <$> mapM (parse partParser "") (filter (/= T.empty) $ T.split (== '/') url)
    <*> mapM (parse queryParamParser "") (filter (/= T.empty) $ T.split (== '&') queryparam)
  where
    partParser :: Parser UrlParts
    partParser = captureParser <|> unitPathParser

    unitPathParser :: Parser UrlParts
    unitPathParser =
      UnitPath . T.pack <$> many1 (noneOf "/")

    captureParser :: Parser UrlParts
    captureParser = do
      _ <- char '{'
      name <- many1 (noneOf ":")
      _ <- char ':'
      captureType <- many1 (noneOf "}")
      _ <- char '}'
      return (Capture (T.pack name) (T.pack captureType))

    queryParamParser :: Parser UrlParts
    queryParamParser = do
      _ <- option False (char '?' >> return True)
      mandatory <- option False (char '*' >> return True)
      name <- many1 (noneOf ":")
      _ <- char ':'
      paramType <- many1 (noneOf "&")
      return (QueryParam (T.pack name) (T.pack paramType) mandatory)

authParser :: Parser ApiParts
authParser = do
  _ <- string "AUTH"
  spaces
  authTyp <- Just <$> authTypeParser
  return $ Auth authTyp
  where
    authTypeParser :: Parser AuthType
    authTypeParser =
      choice
        [ AdminTokenAuth <$ string "AdminTokenAuth",
          TokenAuth <$ string "TokenAuth"
        ]

headerParser :: Parser ApiParts
headerParser = do
  _ <- string "Header"
  spaces
  HeaderT <$> parseHeaderItem
  where
    parseHeaderItem :: Parser HeaderType
    parseHeaderItem = do
      key <- T.pack <$> many1 (noneOf " ")
      spaces
      _ <- char '{'
      value <- T.pack <$> many1 (noneOf "}")
      return $ Header key value

reqParser :: Parser ApiParts
reqParser = do
  reqFormat <-
    T.pack
      <$> try
        ( do
            _ <- string "REQJ"
            spaces
            return "JSON"
        )
      <|> try
        ( do
            _ <- string "REQ"
            spaces
            reqFormat <- T.pack <$> (char '[' *> many1 (noneOf "]") <* char ']')
            spaces
            return reqFormat
        )
  reqType <- T.pack <$> (char '{' *> many1 (noneOf "}") <* char '}')
  return $ Req reqFormat reqType

resParser :: Parser ApiParts
resParser = do
  resFormat <-
    T.pack
      <$> try
        ( do
            _ <- string "RESPJ"
            spaces
            return "JSON"
        )
      <|> ( do
              _ <- string "RESP"
              spaces
              resFormat <- T.pack <$> (char '[' *> many1 (noneOf "]") <* char ']')
              spaces
              return resFormat
          )
  resType <- T.pack <$> (char '{' *> many1 (noneOf "}") <* char '}')
  return $ Res resFormat resType
