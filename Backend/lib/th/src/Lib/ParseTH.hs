import Data.List (find)
import Data.Text (Text, pack, splitOn)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth deriving (Show)

data ApiTT = ApiTT
  { urlParts :: Text,
    apiType :: ApiType,
    authType :: Maybe AuthType,
    headers :: Maybe [(Text, Text)],
    apiReqType :: Maybe Text,
    apiResType :: Text
  }
  deriving (Show)

type Parser = Parsec Void Text

-- ... (previous code)

-- Parser for the entire input
apiTTParser :: Parser ApiTT
apiTTParser = do
  apiType <- apiTypeParser
  space
  urlParts <- pack <$> someTill anySingle space
  lines <- many lineParser
  let (authType, headers, reqType, resType) = processLines lines
  return $ ApiTT urlParts apiType authType headers reqType resType

-- Parser for API CURL
apiCurlParser :: Parser ApiTT
apiCurlParser = apiTTParser

-- Parser for each line in the input
lineParser :: Parser (Maybe AuthType, Maybe (Text, Text), Maybe Text, Text)
lineParser =
  choice
    [ try authParser,
      try headerParser,
      try reqParser,
      resParser
    ]

-- Parser for AUTH directive
authParser :: Parser (Maybe AuthType, Maybe (Text, Text), Maybe Text, Text)
authParser = do
  string "AUTH"
  space
  authType <- Just <$> authTypeParser
  return (authType, Nothing, Nothing, "")

-- Parser for HEADER directive
headerParser :: Parser (Maybe AuthType, Maybe (Text, Text), Maybe Text, Text)
headerParser = do
  string "Header"
  space
  headers <- Just <$> sepBy1 headerParser space
  return (Nothing, headers, Nothing, "")

-- Parser for REQ directive
reqParser :: Parser (Maybe AuthType, Maybe (Text, Text), Maybe Text, Text)
reqParser = do
  string "REQ"
  space
  reqType <- Just <$> reqResParser
  return (Nothing, Nothing, reqType, "")

-- Parser for RESP directive
resParser :: Parser (Maybe AuthType, Maybe (Text, Text), Maybe Text, Text)
resParser = do
  string "RESP"
  space
  resType <- reqResParser
  return (Nothing, Nothing, Nothing, resType)

-- ... (previous code)

-- Helper function to parse multiple lines
parseApiT :: Text -> Either ParseError [ApiTT]
parseApiT input = mapM (parse parseLineAPI "") $ T.split (== '\n') input

-- Main function
main :: IO ()
main = do
  let input = "POST /org/driver/\n  AUTH TokenAuth\n  Header mId (Id Merchant)\n  Header vt VehicleType\n  REQ DTB.TicketBody\n  RESP DTB.TicketServiceVerificationResp"
  let parsedApis = parseApiT input
  case parsedApis of
    Left err -> print err
    Right apiList -> mapM_ print apiList
