module Utils.Parse where

import DBQuery.Types
import Data.Aeson as A
import Data.Aeson.Types (Parser, emptyArray)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Text.Casing (camel)
import Prelude (read)

parseStreamEntryId :: ByteString -> EL.KVDBStreamEntryID
parseStreamEntryId bs =
  -- "number-number" is redis entry id invariant
  let (ms, sq) =
        bimap (read . T.unpack) (read . T.unpack)
          . splitOn "-"
          . TE.decodeUtf8With TE.lenientDecode
          $ bs
   in EL.KVDBStreamEntryID ms sq
  where
    splitOn dilim = (\tup -> (fst tup, T.drop 1 $ snd tup)) . T.breakOn dilim

parseUpdateValue :: A.Value -> Parser (Text, A.Value)
parseUpdateValue = A.withObject "update key value pair" $ \o ->
  (,) <$> (o .: "value0") <*> (o .: "value1")

parseDeleteCommandValues ::
  A.Value -> Parser Where
parseDeleteCommandValues = A.withObject "DBDeleteCommand: Where clause object" $ \o -> do
  w <- o .: "value0"
  if w == ("where" :: Text)
    then decodeWhere =<< o .: "value1"
    else fail "Expected where clause"

parseUpdateCommandValues :: A.Value -> Parser ([Set], Where)
parseUpdateCommandValues = A.withArray "DBUpdateCommand" $ \v -> do
  case V.toList v of
    [updVals, whereClauseObj] -> liftA2 (,) (parseUpdateSetClause updVals) (parseWhereValuePairs whereClauseObj)
    _ -> fail "Expected UpdateCommand updateVals and whereVals"
  where
    parseUpdateSetClause updVals = parseSetClause =<< parseUpdateValuePairs updVals
    parseUpdateValuePairs o = traverse parseUpdateValue =<< parseJSON o

parseCreateCommandValues :: A.Value -> Parser [TermWrap]
parseCreateCommandValues = A.withObject "DBCreateCommand" $ \o -> do
  let termWrapList = (\(k, v) -> TermWrap (Column k) (Value v)) <$> HM.toList o
  case termWrapList of
    [] -> fail "Expected at least one term wrap for CreateCommand"
    _ -> pure termWrapList

parseSetClause :: [(Text, A.Value)] -> Parser [Set]
parseSetClause kvPairs = forM kvPairs $ \(key, value) -> do
  pure $ Set (Column key) (Value value)

parseWhereValuePairs :: A.Value -> Parser Where
parseWhereValuePairs = A.withArray "Where clause list" $ \v -> do
  case V.toList v of
    [whereObj] ->
      ( A.withObject "Where clause object" $ \o -> do
          w <- o .: "value0"
          if w == ("where" :: Text)
            then decodeWhere =<< o .: "value1"
            else fail "Expected where clause"
      )
        whereObj
    _ -> fail "Expected where clause as a list with single element"

decodeWhere ::
  A.Value ->
  Parser [Clause]
decodeWhere whereClauseObj = (\x -> pure [x]) =<< decodeClause whereClauseObj

decodeClause ::
  A.Value ->
  Parser Clause
decodeClause = foldWhere'
  where
    foldWhere' :: A.Value -> Parser Clause
    foldWhere' obj = case obj of
      (A.Object hm) ->
        if HM.member @Text "$and" hm
          then
            ( A.withArray "DBUpdateCommand And" $ \v -> do
                clauses <- mapM foldWhere' (V.toList v)
                return $ And clauses
            )
              (HM.lookupDefault emptyArray "$and" hm)
          else
            if HM.member "$or" hm
              then
                ( A.withArray "DBUpdateCommand Or" $ \v -> do
                    clauses <- mapM foldWhere' (V.toList v)
                    return $ Or clauses
                )
                  (HM.lookupDefault emptyArray "$or" hm)
              else decodeTerm obj
      _ -> fail "unable to decode"

decodeTerm :: A.Value -> Parser Clause
decodeTerm = \case
  A.Object hm -> do
    (key, val) <- getSingleKeyValue hm
    let keyCamel = (T.pack . camel . T.unpack) key
    case val of
      Just (A.Object obj) -> do
        (operation, mValue) <- getSingleKeyValue obj
        case (operation, mValue) of
          ("$in", Just value) -> do
            case value of
              A.Array vecList -> do
                let inList = V.toList vecList
                clauseList <- mapM (\v -> parseFieldAndGetClause v keyCamel) inList
                return $ Or $ map (\(TermWrap col fieldValue) -> Is col (Eq fieldValue)) clauseList
              _ -> fail "Expecting list - Decoding failed at term $in"
          ("$gt", Just value) -> do
            (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel

            return $ Is column (GreaterThan fieldValue)
          ("$gte", Just value) -> do
            (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel
            return $ Is column (GreaterThanOrEq fieldValue)
          ("$lt", Just value) -> do
            (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel
            return $ Is column (LessThan fieldValue)
          ("$lte", Just value) -> do
            (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel
            return $ Is column (LessThanOrEq fieldValue)
          ("$notIn", Just value) -> do
            case value of
              A.Array vecList -> do
                let inList = V.toList vecList
                clauseList <- mapM (\v -> parseFieldAndGetClause v keyCamel) inList
                return $ And $ map (\(TermWrap col fieldValue) -> Is col (Not $ Eq fieldValue)) clauseList
              _ -> fail "Expecting an Array - Error decoding term"
          ("$ne", Just value) -> do
            (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel
            return $ Is column (Not $ Eq fieldValue)
          ("$not", Just value) -> (\(Is col term) -> Is col (Not term)) <$> decodeTerm (A.object [keyCamel A..= value])
          _ -> fail "Expecting term constructor - Error decoding term"
      Just value -> do
        (TermWrap column fieldValue) <- parseFieldAndGetClause value keyCamel
        return $ Is column (Eq fieldValue)
      _ -> fail "Expecting term object - Error decoding term"
  _ -> fail "Expecting Clause object - Error decoding Clause"
  where
    getSingleKeyValue hm = case HM.keys hm of
      [k] -> return (k, HM.lookup k hm)
      _ -> fail "Unable to decode term - Expecting object with single key"

parseFieldAndGetClause :: A.Value -> Text -> Parser TermWrap
parseFieldAndGetClause obj fieldName = pure $ TermWrap (Column fieldName) (Value obj)
