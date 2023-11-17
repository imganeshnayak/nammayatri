{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Mobius.Utils.TH.QueriesTh where

import Database.Beam (in_, sqlBool_, (&&?.), (/=?.), (<-.), (<.), (<=.), (==.), (==?.), (>.), (>=.))
import Database.Beam.Query (val_)
import EulerHS.Prelude hiding (EQ, GT, LT)
import Language.Haskell.TH
import Mobius.Constants.APIErrorCode
  ( internalServerErr,
    requestNotFound,
  )
import Mobius.Utils.DB.Queries
  ( createOneAndReturnWithError,
    createOneWithError,
    deleteWithErr,
    findAllWithErr,
    findOneMaybe,
    findOneWithErr,
    updateRecordWithErr,
  )
import Mobius.Utils.TH.UtilsTh (capitalized, extractConstructors, extractRecFields, toLowerMinusLastChar, (+++))

data QT = EQ | NE | GT | LT | GTE | LTE | IN

data QI = QI Name QT

--TODO eliminate the need for the user input for the first two arguments
mkCreateAndReturnQuery :: Name -> Name -> Name -> Q [Dec]
mkCreateAndReturnQuery tableName insertExp entityName = do
  let fnName = mkName ("create" <> init (nameBase entityName))
      varName = mkName (toLowerMinusLastChar (nameBase entityName))
  return
    [ FunD
        fnName
        [ Clause
            [VarP varName]
            ( NormalB
                ( AppE
                    ( AppE
                        (AppE (VarE 'createOneAndReturnWithError) (UnboundVarE tableName))
                        (AppE (VarE insertExp) (VarE varName))
                    )
                    (VarE 'internalServerErr)
                )
            )
            []
        ]
    ]

mkCreateQuery :: Name -> Name -> Name -> Q [Dec]
mkCreateQuery tableName insertExp entityName = do
  let fnName = mkName ("create" <> init (nameBase entityName))
      varName = mkName (toLowerMinusLastChar (nameBase entityName))
  return
    [ FunD
        fnName
        [ Clause
            [VarP varName]
            ( NormalB
                ( AppE
                    (AppE (VarE 'createOneWithError) (UnboundVarE tableName))
                    (AppE (VarE insertExp) (VarE varName))
                )
            )
            []
        ]
    ]

--- Templatized Make Queries for find

mkFindOneQueries :: Name -> Name -> [[QI]] -> Q [Dec]
mkFindOneQueries entityName dbName fieldNms = concat <$> forM fieldNms (mkFindOne entityName dbName)

mkFindAllWithQueries :: Name -> Name -> [[QI]] -> Q [Dec]
mkFindAllWithQueries entityName dbName fieldNms = concat <$> forM fieldNms (mkFindAllWithErr entityName dbName)

mkFindOneErrQueries :: Name -> Name -> [[QI]] -> Q [Dec]
mkFindOneErrQueries entityName dbName fieldNms = concat <$> forM fieldNms (mkFindOneWithErr entityName dbName)

mkFindOne :: Name -> Name -> [QI] -> Q [Dec]
mkFindOne entityName dbName fieldNm = do
  let fnName = "findMaybe" <> ((init . nameBase) entityName) <> "By" <> funcName fieldNm
  mkFindOneWithFnName entityName dbName fieldNm fnName

mkFindOneWithFnName :: Name -> Name -> [QI] -> String -> Q [Dec]
mkFindOneWithFnName _ dbName fieldNm fName = do
  let fnName = mkName $ fName
      x = mkName "x"
      variableNames = VarP . mkName . nameBase . getFieldName <$> fieldNm
  predicateExpr <- LamE [VarP x] <$> query x fieldNm
  let bodyExpr = (AppE (AppE (VarE 'findOneMaybe) (VarE dbName)) predicateExpr)
  return [FunD fnName [Clause variableNames (NormalB bodyExpr) []]]

mkFindAllWithErr :: Name -> Name -> [QI] -> Q [Dec]
mkFindAllWithErr entityName dbName fieldNm = do
  let fnName = mkName $ "findAll" <> ((init . nameBase) entityName) <> "By" <> funcName fieldNm
      x = mkName "x"
      variableNames = VarP . mkName . nameBase . getFieldName <$> fieldNm
  predicateExpr <- LamE [VarP x] <$> query x fieldNm
  let bodyExpr = (AppE (AppE (VarE 'findAllWithErr) (VarE dbName)) predicateExpr)
  return [FunD fnName [Clause variableNames (NormalB bodyExpr) []]]

mkFindOneWithErr :: Name -> Name -> [QI] -> Q [Dec]
mkFindOneWithErr entityName dbName fieldNm = do
  let fName = "find" <> ((init . nameBase) entityName) <> "By" <> funcName fieldNm
  mkFindOneWithErrWithFnName entityName dbName fieldNm fName

mkFindOneWithErrWithFnName :: Name -> Name -> [QI] -> String -> Q [Dec]
mkFindOneWithErrWithFnName _ dbName fieldNm fName = do
  let fnName = mkName $ fName
      x = mkName "x"
      variableNames = VarP . mkName . nameBase . getFieldName <$> fieldNm
  predicateExpr <- LamE [VarP x] <$> query x fieldNm
  let bodyExpr = (AppE (AppE (AppE (VarE 'findOneWithErr) (VarE dbName)) predicateExpr) (VarE 'requestNotFound))
  return [FunD fnName [Clause variableNames (NormalB bodyExpr) []]]

-- helper functions
funcName :: [QI] -> String
funcName (hd : tl) = (foldl' (\b t -> b <> "And" <> (getFunctionPrefix t)) (getFunctionPrefix hd) tl)
funcName [] = fail "Invalid Make Find"

query :: Name -> [QI] -> Q Exp
query x (hd : tl) = pure (foldl' (\b a -> InfixE (Just b) (VarE '(&&?.)) (Just $ getQueryStatement x a)) (getQueryStatement x hd) tl)
query _ [] = fail "Invalid Make Find"

getFunctionPrefix :: QI -> String
getFunctionPrefix (QI nm qt) =
  let name = (capitalized (nameBase nm))
   in case qt of
        EQ -> name
        NE -> "Not" <> name
        GT -> "GreaterThan" <> name
        LT -> "LessThan" <> name
        GTE -> "GreaterThanEqual" <> name
        LTE -> "LessThanEqual" <> name
        IN -> name <> "s"

getQueryStatement :: Name -> QI -> Exp
getQueryStatement x (QI fieldNm qt) =
  let variableName = (mkName (nameBase fieldNm))
   in case qt of
        EQ -> (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(==?.)) (Just (AppE (VarE 'val_) (VarE variableName))))
        NE -> (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(/=?.)) (Just (AppE (VarE 'val_) (VarE variableName))))
        GT -> (AppE (VarE 'sqlBool_) (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(>.)) (Just (AppE (VarE 'val_) (VarE variableName)))))
        LT -> (AppE (VarE 'sqlBool_) (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(<.)) (Just (AppE (VarE 'val_) (VarE variableName)))))
        GTE -> (AppE (VarE 'sqlBool_) (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(>=.)) (Just (AppE (VarE 'val_) (VarE variableName)))))
        LTE -> (AppE (VarE 'sqlBool_) (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(<=.)) (Just (AppE (VarE 'val_) (VarE variableName)))))
        IN -> (AppE (VarE 'sqlBool_) (AppE (AppE (VarE 'in_) (AppE (VarE fieldNm) (VarE x))) (InfixE (Just (VarE 'val_)) (VarE '(<$>)) (Just (VarE variableName)))))

getFieldName :: QI -> Name
getFieldName (QI fieldNm _) = fieldNm

getUpdateExpression :: Name -> Name -> Q Exp
getUpdateExpression entityName y = do
  let x = mkName "z"
  recFields <- extractRecFields . head . extractConstructors <$> reify entityName
  LamE [VarP x] <$> query' x recFields
  where
    updateExpr x fieldNm = InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(<-.)) (Just (AppE (VarE 'val_) ((AppE (VarE fieldNm) (VarE y)))))
    query' x (hd : tl) = pure (foldl' (\b a -> InfixE (Just b) (VarE '(<>)) (Just $ updateExpr x a)) (updateExpr x hd) tl)
    query' _ [] = fail "Invalid Make Find"

mkUpdateQuery :: Name -> Name -> [Name] -> String -> Q [Dec]
mkUpdateQuery entityName dbName primaryKeyNms fName = do
  let fnName = mkName fName
      x = mkName "x"
      rt = mkName "rt"
  predicateExpr <- LamE [VarP x] <$> query' rt x primaryKeyNms
  updateExpressions <- getUpdateExpression entityName rt
  let bodyExpr = (AppE (AppE (AppE (AppE (VarE 'updateRecordWithErr) (VarE dbName)) updateExpressions) predicateExpr) (VarE 'requestNotFound))
  return [FunD fnName [Clause [VarP rt] (NormalB bodyExpr) []]]
  where
    eQuery rt x fieldNm = (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(==?.)) (Just (AppE (VarE 'val_) ((AppE (VarE fieldNm) (VarE rt))))))
    query' rt x (hd : tl) = pure (foldl' (\b a -> InfixE (Just b) (VarE '(&&?.)) (Just $ eQuery rt x a)) (eQuery rt x hd) tl)
    query' _ _ [] = fail "Invalid Update Find"

mkDeleteQuery :: Name -> Name -> [Name] -> String -> Q [Dec]
mkDeleteQuery _ dbName primaryKeyNms fName = do
  let fnName = mkName fName
      x = mkName "x"
      variableNames = VarP . mkName . nameBase <$> primaryKeyNms
  predicateExpr <- LamE [VarP x] <$> query' x primaryKeyNms
  let bodyExpr = (AppE (AppE (AppE (VarE 'deleteWithErr) (VarE dbName)) predicateExpr) (VarE 'requestNotFound))
  return [FunD fnName [Clause variableNames (NormalB bodyExpr) []]]
  where
    eQuery x fieldNm = (InfixE (Just (AppE (VarE fieldNm) (VarE x))) (VarE '(==.)) (Just (AppE (VarE 'val_) (VarE ((mkName (nameBase fieldNm)))))))
    query' x (hd : tl) = pure (foldl' (\b a -> InfixE (Just b) (VarE '(&&?.)) (Just $ eQuery x a)) (eQuery x hd) tl)
    query' _ [] = fail "Invalid Delete Find"

mkQueries :: Name -> Name -> Name -> [Name] -> Q [Dec]
mkQueries entityName dbName insertExpr pmKey =
  do
    mkCreateAndReturnQuery dbName insertExpr entityName
    +++ mkFindOneWithErrWithFnName entityName dbName (flip QI EQ <$> pmKey) ("find" <> (init . nameBase) entityName)
    +++ mkUpdateQuery entityName dbName pmKey ("update" <> (init . nameBase) entityName)
    +++ mkDeleteQuery entityName dbName pmKey ("delete" <> (init . nameBase) entityName)
