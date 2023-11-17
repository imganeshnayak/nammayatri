{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib.RoutesTh where

import Data.HashMap.Lazy (singleton)
import EulerHS.Prelude
import Language.Haskell.TH
import Lib.UtilsTh (foldREmptyList, (+++))
import Mobius.Utils.Routes (defaultFlowWithTrace)
import Servant

type DefaultRespHeaders =
  Headers '[Header "x-requestid" Text, Header "x-sessionid" Text]

type (./) = (:>)

infixr 4 ./

type (++.) = (:<|>)

infixr 3 ++.

type (.:) = Capture

infixr 5 .:

type (.?) = QueryParam

infixr 5 .?

type DefaultResp a b = Vault ./ a '[JSON] (DefaultRespHeaders b)

type DefReqBody = ReqBody '[JSON]

mkRoutes :: Name -> [[Name]] -> Q [Dec]
mkRoutes apiDec apiHandlerList =
  do
    mkProxyDec apiDec
    +++ concatMapM mkHandlerFuncDec (concat apiHandlerList)
    +++ mkHandlerFuncs apiHandlerList

mkProxyDec :: Name -> Q [Dec]
mkProxyDec apiDec = do
  let name = mkName "apis"
  return [SigD name (AppT (ConT ''Proxy) (ConT apiDec)), ValD (VarP name) (NormalB (ConE 'Proxy)) []]

mkHandlerFuncs :: [[Name]] -> Q [Dec]
mkHandlerFuncs apiHandlerList = do
  let name = mkName "apiServer"
  bodyExpr <- foldREmptyList (\a b -> (\x -> InfixE x (ConE '(:<|>)) (Just b)) . Just <$> (getInternalQueryHandlerList a)) getInternalQueryHandlerList apiHandlerList
  return [FunD name ([Clause [VarP keyName] (NormalB bodyExpr) []])]
  where
    keyName = mkName "apiKey"
    getAppExpr apiHndlr = (AppE (VarE (mkName (nameBase apiHndlr <> "Handler"))) (VarE keyName))
    getInternalQueryHandlerList :: [Name] -> Q Exp
    getInternalQueryHandlerList = foldREmptyList (\a b -> pure (InfixE (Just (getAppExpr a)) (ConE '(:<|>)) (Just b))) (pure . getAppExpr)

mkHandlerFuncDec :: Name -> Q [Dec]
mkHandlerFuncDec apiHndlr = do
  noOfParams <- funcParamCnt apiHndlr
  let name = mkName (nameBase apiHndlr <> "Handler")
      vars = createVar noOfParams
      key = mkName "handlerKey"
      vault = mkName "handlerVault"
      varPs = VarP <$> ([key] <> vars <> [vault])
      funcExpr = foldl' AppE (VarE apiHndlr) $ (VarE <$> vars)
      bodyExpr = (AppE (AppE (AppE (AppE (VarE 'defaultFlowWithTrace) (VarE key)) (VarE vault)) (InfixE (Just (ConE 'Just)) (VarE '($)) (Just (AppE (AppE (VarE 'singleton) (LitE (StringL (nameBase apiHndlr)))) (LitE (StringL (nameBase apiHndlr))))))) funcExpr)
  return [FunD name ([Clause varPs (NormalB bodyExpr) []])]
  where
    createVar 0 = []
    createVar count = mkName ("var_" <> (show count)) : createVar (count - 1)

funcParamCnt :: Name -> Q Int
funcParamCnt name = do
  fInfo <- reify name
  case fInfo of
    VarI _ fType _ -> return $ countTypesInFunc fType
    _ -> fail "Invalid Function Declaration"

countTypesInFunc :: Language.Haskell.TH.Type -> Int
countTypesInFunc t = case t of
  AppT t1 t2 -> (countTypesInFunc t1 + countTypesInFunc t2)
  AppKindT t1 _ -> countTypesInFunc t1
  SigT t1 _ -> countTypesInFunc t1
  ParensT t1 -> countTypesInFunc t1
  ForallT _ _ t1 -> countTypesInFunc t1
  InfixT t1 _ t2 -> (countTypesInFunc t1 + countTypesInFunc t2)
  UInfixT t1 _ t2 -> (countTypesInFunc t1 + countTypesInFunc t2)
  ArrowT -> 1
  _ -> 0
