{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Mobius.Utils.TH.QueryExpressionsTh where

import BasicPrelude (Foldable (foldl'), Maybe (Just), Monad (return), Monoid (mempty), head, init, tail, ($), (.), (<$>), (<>))
import Control.Lens ((^.))
import Database.Beam (modifyTableFields, setEntityName, tableModification, (<-.))
import Database.Beam.Query (insertExpressions, val_)
import Database.Beam.Schema.Tables (setEntitySchema)
import Language.Haskell.TH
import Mobius.Utils.TH.UtilsTh (extractConstructors, extractRecFields, extractSimpleRecFields, toLowerMinusLastChar)

mkEmod :: Name -> Q [Dec]
mkEmod name = do
  names <- extractRecFields . head . extractConstructors <$> reify name
  let fnName = mkName (toLowerMinusLastChar (nameBase name) <> "EMod")
      schemaName = mkName "schema"
      tableName = mkName "tableName"
      recExps = (\name' -> (name', LitE (StringL (tail (nameBase name'))))) <$> names
      bodyExpr =
        InfixE
          (Just (AppE (VarE 'setEntitySchema) (AppE (ConE 'Just) (UnboundVarE schemaName))))
          (VarE '(<>))
          ( Just
              ( InfixE
                  (Just (AppE (VarE 'setEntityName) (UnboundVarE tableName)))
                  (VarE '(<>))
                  ( Just
                      ( AppE
                          (VarE 'modifyTableFields)
                          (RecUpdE (VarE 'tableModification) recExps)
                      )
                  )
              )
          )
  return [FunD fnName [Clause [VarP tableName, VarP schemaName] (NormalB bodyExpr) []]]

mkInsertExpression :: Name -> Q [Dec]
mkInsertExpression name = do
  names' <- extractRecFields . head . extractConstructors <$> reify name
  let names = extractSimpleRecFields names'
      fnName = mkName "insertExpressions"
      cs = mkName "cs"
      toRowExpression = mkName "toRowExpression"
      typeName = mkName $ init (nameBase name)
      bodyExpr =
        ( AppE
            (VarE 'insertExpressions)
            (InfixE (Just (VarE toRowExpression)) (VarE '(<$>)) (Just (VarE cs)))
        )
      foldFn exp1' = AppE exp1' . AppE (VarE 'val_) . VarE
      recExps = foldl' (foldFn) (ConE typeName) names
      fieldPats = (\name' -> (name', VarP name')) <$> names
      funDec = [FunD toRowExpression [Clause [RecP typeName fieldPats] (NormalB recExps) []]]
  return [FunD fnName [Clause [VarP cs] (NormalB bodyExpr) funDec]]

mkUpdateExpression :: Name -> Q [Dec]
mkUpdateExpression name = do
  names' <- extractRecFields . head . extractConstructors <$> reify name
  let names = extractSimpleRecFields names'
      rt = mkName "rt"
      fnName = mkName "updateExpressions"
      fieldPats = (\name' -> (name', VarP name')) <$> names
      typeName = mkName $ init (nameBase name)
      foldFn exp1' = InfixE (Just exp1') (VarE '(<>)) . Just
      bodyExp = ((\a -> InfixE (Just (VarE a)) (VarE '(<-.)) (Just (AppE (VarE 'val_) (InfixE (Just (VarE rt)) (VarE '(^.)) (Just (VarE $ mkName $ tail $ nameBase a)))))) <$> names)
      recExps = foldl' (foldFn) (VarE 'mempty) bodyExp

  return [FunD fnName [Clause [VarP rt, RecP typeName fieldPats] (NormalB recExps) []]]
