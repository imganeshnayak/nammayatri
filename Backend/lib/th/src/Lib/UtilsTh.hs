{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Lib.UtilsTh where

import Data.Bool
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Prelude

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth deriving (Show)

data HeaderType = Header Text Text deriving (Show)

type Apis = [ApiTT]

data ApiTT = ApiTT
  { urlParts :: [UrlParts],
    apiType :: ApiType,
    authType :: Maybe AuthType,
    header :: [HeaderType],
    apiReqType :: Maybe Text,
    apiResType :: Text
  }
  deriving (Show)

data ApiParts = ApiTU ApiType [UrlParts] | HeaderType HeaderType | Auth (Maybe AuthType) | Req Text Text | Res Text Text deriving (Show)

apiParserT :: Text -> Either ParseError [ApiParts]
apiParserT input =
  mapM (parse lineParserAPI "") (filter (/= T.empty) $ T.split (== '\n') input)

lineParserAPI :: Parser ApiParts
lineParserAPI = try apiTypeAndURLParser <|> try authParser <|> try headerParser <|> reqParser <|> resParser

apiTypeParser :: Parser ApiType
apiTypeParser =
  choice
    [ GET <$ string "GET",
      POST <$ string "POST",
      PUT <$ string "PUT",
      DELETE <$ string "DELETE"
    ]

apiTypeAndURLParser :: Parser ApiParts
apiTypeAndURLParser = do
  apiType <- apiTypeParser
  _ <- space
  rawUrl <- T.pack <$> many1 (noneOf "")
  url <- either (const (fail "Bla")) pure $ parseUrl rawUrl
  return $ ApiTU apiType url

authParser :: Parser ApiParts
authParser = do
  _ <- string "AUTH"
  _ <- space
  authType <- Just <$> authTypeParser
  return $ Auth authType

authTypeParser :: Parser AuthType
authTypeParser =
  choice
    [ AdminTokenAuth <$ string "AdminTokenAuth",
      TokenAuth <$ string "TokenAuth"
    ]

headerParser :: Parser ApiParts
headerParser = do
  _ <- string "Header"
  _ <- char ' '
  HeaderType <$> parseHeaderItem

parseHeaderItem :: Parser HeaderType
parseHeaderItem = do
  key <- T.pack <$> many1 (noneOf " ")
  _ <- char ' '
  _ <- char '('
  value <- T.pack <$> many1 (noneOf ")")
  return $ Header key value

reqParser :: Parser ApiParts
reqParser = do
  reqFormat <-
    T.pack
      <$> try
        ( do
            _ <- string "REQJ"
            _ <- space
            return "JSON"
        )
      <|> try
        ( do
            _ <- string "REQ"
            _ <- space
            reqFormat <- T.pack <$> (char '[' *> many1 (noneOf "]") <* char ']')
            _ <- space
            return reqFormat
        )
  reqType <- T.pack <$> (char '(' *> many1 (noneOf ")") <* char ')')
  return $ Req reqFormat reqType

resParser :: Parser ApiParts
resParser = do
  resFormat <-
    T.pack
      <$> try
        ( do
            _ <- string "RESPJ"
            _ <- space
            return "JSON"
        )
      <|> ( do
              _ <- string "RESP"
              _ <- space
              resFormat <- T.pack <$> (char '[' *> many1 (noneOf "]") <* char ']')
              _ <- space
              return resFormat
          )
  resType <- T.pack <$> (char '(' *> many1 (noneOf ")") <* char ')')
  return $ Res resFormat resType

parseUrl :: Text -> Either ParseError [UrlParts]
parseUrl input = do
  let (url, queryparam) = T.breakOnEnd "?" input
  (<>) <$> mapM (parse partParser "") (filter (/= T.empty) $ T.split (== '/') url)
    <*> mapM (parse queryParamParser "") (T.split (== '&') queryparam)

partParser :: Parser UrlParts
partParser = captureParser <|> unitPathParser

unitPathParser :: Parser UrlParts
unitPathParser =
  UnitPath . T.pack <$> many1 (noneOf "/?")

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
  mandatory <- option False (char '*' >> return True)
  name <- many1 (noneOf ":")
  _ <- char ':'
  paramType <- many1 (noneOf "&")
  return (QueryParam (T.pack name) (T.pack paramType) mandatory)
