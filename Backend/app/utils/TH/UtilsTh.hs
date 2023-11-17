{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Mobius.Utils.TH.UtilsTh where

import Data.Char (toLower, toUpper)
import Data.List ((\\))
import Data.List.Split (splitOn)
import EulerHS.Prelude (Applicative (liftA2), Bool (False, True), Char, Monad ((>>=)), MonadFail (fail), Semigroup ((<>)), Show, String, filter, filterM, foldlM, foldrM, head, init, intercalate, last, not, return, show, snd, ($), (.), (<$>), (<&>), (==))
import qualified GHC.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ()
import Mobius.Utils.Utils (removeDuplicatesFromList)

extractProjectName :: Info -> Name
extractProjectName (TyConI (DataD _ tyName _ _ _ _)) = tyName --head (splitOn "." (nameBase tyName))
extractProjectName _ = mkName "InvalidProject"

extractConstructors :: Info -> [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors (TyConI (NewtypeD _ _ _ _ cons _)) = [cons]
extractConstructors _ = []

extractRecFields :: Con -> [Name]
extractRecFields (RecC _ bangs) = handleVarBang <$> bangs where handleVarBang (a, _, _) = a
extractRecFields _ = []

isRecordType :: Con -> Bool
isRecordType (RecC _ _) = True
isRecordType (_) = False

extractSimpleRecFields :: [Name] -> [Name]
extractSimpleRecFields names = mkName . simpleName <$> (nameBase <$> names) where simpleName fullName = last (splitOn "." fullName)

toLowerMinusLastChar :: [Char] -> [Char]
toLowerMinusLastChar name' = case init name' of
  (firstChar : restChars) -> toLower firstChar : restChars
  x -> x

(+++) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(+++) = liftA2 (<>)

capitalized :: String -> String
capitalized (x : xs) = (toUpper x) : xs
capitalized [] = []

data FieldInfo = FieldInfo
  { internalFields :: [Name],
    internalRecords :: [Name],
    internalRecordLists :: [Name],
    internalEnums :: [Name]
  }
  deriving (Show)

-- TODO 1.  infer the type of the primary key

extractRecFieldsAndTypes :: Con -> [(Name, Name)]
extractRecFieldsAndTypes (RecC _ bangs) = handleVarBang <$> bangs
  where
    handleVarBang (fieldName, _, fieldType) = (fieldName, extractSimpleFieldType fieldType)
    --     extractSimpleFieldType (AppT _ (AppT ListT ty)) = (AppT ListT ty)
    extractSimpleFieldType (AppT _ (AppT (ListT) (ConT ty))) = mkName ((show ty) <> ".List")
    extractSimpleFieldType (AppT (ListT) (ConT ty)) = mkName ((show ty) <> ".List")
    extractSimpleFieldType (AppT _ (AppT _ (ConT ty))) = ty
    extractSimpleFieldType (AppT _ (ConT ty)) = ty
    extractSimpleFieldType (ConT ty) = ty
    extractSimpleFieldType _ = mkName "InvalidType"
extractRecFieldsAndTypes _ = []

extractInternalTyNames :: Name -> Q FieldInfo
extractInternalTyNames typeName = do
  fnt <- extractRecFieldsAndTypes . head . extractConstructors <$> reify typeName
  proj <- show . extractProjectName <$> reify typeName
  totalFields <- extractAllInternalTyNames [typeName]
  let proj' = head (splitOn "." proj)
      iFields = (snd <$> filter (\(_, y) -> head (splitOn "." (show y)) == proj') fnt)
      --iFields'    =  checkList' <$> (snd <$> filter (\(_,y) -> head(splitOn "." (show y)) == proj') fnt)
      iListRecordFields = checkList' <$> filter (iList) iFields
      iRecField field = (reify field) <&> isRecordType . head . extractConstructors
      iEnumField field = (reify field) <&> not . isRecordType . head . extractConstructors
      iList field = (("List" `L.elem` (splitOn "." (show field))))
  iRecFields <- filterM iRecField totalFields
  iEnumFileds <- filterM iEnumField totalFields

  return $
    FieldInfo
      totalFields
      (removeDuplicatesFromList $ iRecFields \\ iListRecordFields)
      iListRecordFields
      (removeDuplicatesFromList $ iEnumFileds \\ iListRecordFields)

extractAllInternalTyNames :: [Name] -> Q [Name]
extractAllInternalTyNames (typeName : typenames) = do
  fnt <- extractRecFieldsAndTypes . head . extractConstructors <$> reify typeName
  proj <- show . extractProjectName <$> reify typeName
  let proj' = head (splitOn "." proj)
      iFields = checkList' <$> (snd <$> filter (\(_, y) -> head (splitOn "." (show y)) == proj') fnt)
  fieldInfo <- extractAllInternalTyNames iFields
  remFields <- (extractAllInternalTyNames typenames)
  return (removeDuplicatesFromList $ fieldInfo <> remFields <> iFields)
extractAllInternalTyNames [] = return ([])

checkList' :: Name -> Name
checkList' name =
  if ("List" `L.elem` (splitOn "." (show name)))
    then mkName $ intercalate "." (init $ splitOn "." (show name))
    else name

foldLEmptyList :: (b -> a -> Q b) -> (a -> Q b) -> [a] -> Q b
foldLEmptyList func initFunc (hd : tail) = initFunc hd >>= (\b -> foldlM func b tail)
foldLEmptyList _ _ [] = fail "Non Empty Array"

foldREmptyList :: (a -> b -> Q b) -> (a -> Q b) -> [a] -> Q b
foldREmptyList _ _ [] = fail "Non Empty Array"
foldREmptyList func initFunc x = initFunc (last x) >>= (\b -> foldrM func b (init x))
