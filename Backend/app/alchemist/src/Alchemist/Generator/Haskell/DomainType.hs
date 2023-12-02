module Alchemist.Generator.Haskell.DomainType where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import qualified Data.Text as T
import Kernel.Prelude

generateDomainType :: TableDef -> String
generateDomainType tableDef = do
  let defaultImports = createDefaultImports tableDef

  "{-# LANGUAGE TemplateHaskell #-}\n"
    ++ "module "
    ++ moduleName
    ++ " where\n\n"
    ++ intercalate "\n" (map (\i -> "import qualified " ++ i ++ " as " ++ i) (removeDefaultImports defaultImports moduleName $ imports tableDef))
    ++ "\n"
    ++ intercalate "\n" (map ("import " ++) defaultImports)
    ++ "\n\ndata "
    ++ tableNameHaskell tableDef
    ++ " = "
    ++ tableNameHaskell tableDef
    ++ "\n  { "
    ++ intercalate "\n  , " (map fieldDefToHaskell (fields tableDef))
    ++ "\n  }\n  deriving (Generic, Show)\n\n\n"
    ++ maybe "" generateHaskellTypes (types tableDef)
  where
    moduleName = "Domain.Types." ++ tableNameHaskell tableDef

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter ((/=) moduleName) . filter (`notElem` defaultImports)

-- Convert FieldDef to Haskell field
fieldDefToHaskell :: FieldDef -> String
fieldDefToHaskell fieldDef =
  fieldName fieldDef ++ " :: " ++ haskellType fieldDef

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]

shouldImportUtilsTH :: [TypeObject] -> Bool
shouldImportUtilsTH typeObj =
  any
    ( \case
        TypeObject (_, fields) -> isEnum fields
    )
    typeObj

isEnum :: [(Text, Text)] -> Bool
isEnum [("enum", _)] = True
isEnum _ = False

generateHaskellTypes :: [TypeObject] -> String
generateHaskellTypes typeObj = T.unpack $ T.unlines $ concatMap processType typeObj
  where
    processType :: TypeObject -> [Text]
    processType (TypeObject (typeName, fields))
      | isEnum fields = generateEnum typeName fields
      | otherwise = generateDataStructure typeName fields

    generateEnum :: Text -> [(Text, Text)] -> [Text]
    generateEnum typeName [("enum", values)] =
      let enumValues = T.splitOn "," values
       in ["data " <> typeName <> " = " <> T.intercalate " | " enumValues]
            ++ ["  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)\n\n"]
            ++ ["$(mkBeamInstancesForEnum ''" <> typeName <> ")\n"]
    generateEnum _ _ = error "Invalid enum definition"

    generateDataStructure :: Text -> [(Text, Text)] -> [Text]
    generateDataStructure typeName fields =
      ["data " <> typeName <> " = " <> typeName]
        ++ ["  { " <> T.intercalate ",\n    " (map formatField fields) <> "\n  }"]
        ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema)\n"]

    formatField :: (Text, Text) -> Text
    formatField (fieldName, fieldType) = fieldName <> " :: " <> fieldType
