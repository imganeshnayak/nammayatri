{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.GroupParser where

import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String
import Prelude

data Group = Single Name | Grp [Group] deriving (Show, Data, Typeable)

parseGroupExp :: String -> Q Exp
parseGroupExp str = case parse groupParser "" str of
  Left err -> error $ "Parse error: " ++ show err
  Right group -> dataToExpQ (const Nothing) group
  where
    identifier :: Parser String
    identifier = many1 (alphaNum <|> char '_' <|> char '.')

    groupParser :: Parser Group
    groupParser = skipChars *> (singleName <|> grpList) <* skipChars

    singleName :: Parser Group
    singleName =
      skipChars
        *> ( do
               name <- identifier
               return $ Single (mkName name)
           )
        <* skipChars

    grpList :: Parser Group
    grpList = grpNested <|> grpSingle

    grpSingle :: Parser Group
    grpSingle = do
      skipChars
      _ <- char '['
      skipChars
      n <- singleName
      skipChars
      _ <- char ']'
      skipChars
      return n

    grpNested :: Parser Group
    grpNested = do
      _ <- char '['
      skipChars
      first <- singleName <|> grpNested
      rest <- many (skipChars *> char ',' *> skipChars >> (singleName <|> grpNested))
      skipChars
      _ <- char ']'
      skipChars
      return $ Grp (first : rest)

handlerExp :: QuasiQuoter
handlerExp =
  QuasiQuoter
    { quoteExp = parseGroupExp,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

skipChars :: Parser ()
skipChars = skipMany (char ' ' <|> char '\n' <|> char '\t')
