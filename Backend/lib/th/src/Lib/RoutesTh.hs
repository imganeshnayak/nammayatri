{-# LANGUAGE TemplateHaskell #-}

module Lib.RoutesTh where

import Data.List (nub)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Utils.Common
import Language.Haskell.TH
import Lib.UtilsTh ((+++))
import Servant

data Group = Single Name | Grp [Group]

mkRoutes :: Name -> Name -> Name -> Group -> Q [Dec]
mkRoutes apiDec handlerTypeCons returnTypeCons apiHandlerGrps =
  do
    mkProxyDec apiDec
    +++ mkHandlerFuncDec returnTypeCons apiHandlerGrps
    +++ mkHandlerFuncs apiDec handlerTypeCons apiHandlerGrps

mkProxyDec :: Name -> Q [Dec]
mkProxyDec apiDec = do
  let name = mkName "apisProxy"
  return [SigD name (AppT (ConT ''Proxy) (ConT apiDec)), ValD (VarP name) (NormalB (ConE 'Proxy)) []]

mkHandlerFuncs :: Name -> Name -> Group -> Q [Dec]
mkHandlerFuncs apiDec handlerTypeCons apiHandlers = do
  let name = mkName "handler"
  let bodyExpr = generateHandler apiHandlers
  return [SigD name (AppT (ConT handlerTypeCons) (ConT apiDec)), FunD name ([Clause [] (NormalB bodyExpr) []])]
  where
    getAppExpr apiHndlr = VarE (mkName (nameBase apiHndlr))
    generateHandler (Single name) = getAppExpr name
    generateHandler (Grp grps) = ParensE $ foldl1 (\acc e -> UInfixE acc (ConE '(:<|>)) e) (map generateHandler grps)

mkHandlerFuncDec :: Name -> Group -> Q [Dec]
mkHandlerFuncDec returnTypeCons (Grp grps) = nub <$> (concatMapM (mkHandlerFuncDec returnTypeCons) grps)
mkHandlerFuncDec returnTypeCons (Single apiHndlr) = do
  (noOfParams, fType) <- funcParamCntAndType apiHndlr
  let name = mkName (nameBase apiHndlr)
      vars = createVar noOfParams
      varPs = VarP <$> vars
      funcExpr = foldl' AppE (VarE apiHndlr) $ (VarE <$> vars)
      bodyExpr = AppE (VarE 'withFlowHandlerAPI) $ (funcExpr)
  return [SigD name (correctLastType returnTypeCons fType), FunD name ([Clause varPs (NormalB bodyExpr) []])]
  where
    createVar 0 = []
    createVar count = mkName ("var_" <> (show count)) : createVar (count - 1)

funcParamCntAndType :: Name -> Q (Int, Language.Haskell.TH.Type)
funcParamCntAndType name = do
  fInfo <- reify name
  case fInfo of
    VarI _ fType _ -> return $ (countTypesInFunc fType, fType)
    _ -> fail "Invalid Function Declaration"

countTypesInFunc :: Language.Haskell.TH.Type -> Int
countTypesInFunc t = case t of
  AppT t1 t2 -> countTypesInFunc t1 + countTypesInFunc t2
  AppKindT t1 _ -> countTypesInFunc t1
  SigT t1 _ -> countTypesInFunc t1
  ParensT t1 -> countTypesInFunc t1
  ForallT _ _ t1 -> countTypesInFunc t1
  InfixT t1 _ t2 -> countTypesInFunc t1 + countTypesInFunc t2
  UInfixT t1 _ t2 -> countTypesInFunc t1 + countTypesInFunc t2
  ArrowT -> 1
  _ -> 0

correctLastType :: Name -> Language.Haskell.TH.Type -> Language.Haskell.TH.Type
correctLastType tp (ForallT _ _ t1) = correctLastType tp t1
correctLastType tp (AppT (AppT ArrowT m) t) = (AppT (AppT ArrowT m) (correctLastType tp t))
correctLastType tp t = changeType tp t

changeType :: Name -> Language.Haskell.TH.Type -> Language.Haskell.TH.Type
changeType tp (AppT _ t) = AppT (ConT tp) t
changeType _ t = t

--- FOR TESTING ---

printHaskellCode :: IO ()
printHaskellCode = do
  decs <- runQ routesQDec
  liftIO $ putStrLn $ pprint decs

type MobiusAPIs = Text

type ApiType = Text

mobiusAPIs :: Proxy MobiusAPIs
mobiusAPIs = Proxy

checkApp1 :: L.Flow Text
checkApp1 = do
  pure "App is UP"

checkApp2 :: L.Flow Text
checkApp2 = do
  pure "App is UP"

checkApp3 :: L.Flow Text
checkApp3 = do
  pure "App is UP"

routesQDec :: Q [Dec]
routesQDec = mkRoutes ''MobiusAPIs ''ApiType ''Text (Grp [Grp [Grp [Single 'checkApp1, Single 'checkApp2], Single 'checkApp2], Grp [Single 'checkApp3, Single 'checkApp2]])
