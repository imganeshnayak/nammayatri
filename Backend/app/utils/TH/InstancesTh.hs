{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Mobius.Utils.TH.InstancesTh where

import BasicPrelude (Foldable (foldl'))
import Control.Lens.Combinators (makeLenses)
import Data.Aeson (FromJSON, SumEncoding (UntaggedValue), ToJSON, Value, defaultOptions, genericParseJSON, genericToJSON, omitNothingFields, parseJSON, sumEncoding, toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Database.Beam (FromBackendRow, HasSqlEqualityCheck)
import Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Backend.Types (BeamBackend)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.FromField (FromField (fromField), ResultError (ConversionFailed, UnexpectedNull), returnError)
import GHC.Generics hiding (C, NoSourceStrictness, NoSourceUnpackedness)
import Language.Haskell.TH as TH
import Mobius.Utils.TH.UtilsTh (FieldInfo (..), extractInternalTyNames, (+++))
import Mobius.Utils.Utils (genericFromField, stripAllLensPrefixOptions)
import Text.Read (readMaybe)
import Prelude

-- super function that infers types & generates the rest
mkInstances :: Name -> [Name] -> Q [Dec]
mkInstances typeName pkInfo = do
  fieldInfo <- extractInternalTyNames typeName
  deriveMainDbRecordInstances typeName pkInfo
    +++ makeLenses typeName
    +++ makeLensesMap (internalRecords fieldInfo)
    +++ makeLensesMap (internalRecordLists fieldInfo)
    +++ deriveDbRecordListInstancesMap (internalRecordLists fieldInfo)
    +++ deriveDbRecordInstancesMap (internalRecords fieldInfo)
    +++ deriveDbEnumInstancesMap (internalEnums fieldInfo)

--- Moving to a better format with Quotes and Splices

simpleInstanceDec :: Name -> Name -> Q [Dec]
simpleInstanceDec instanceName typeName = [d|deriving instance $t $a|]
  where
    a = conT typeName
    t = conT instanceName

deriveMultipleSimpleInstances :: Name -> [Name] -> Q [Dec]
deriveMultipleSimpleInstances typeName instances = mapM (`simpleInstanceDec` typeName) instances <&> concat

fromJSONRecLensDec :: Name -> Q [Dec]
fromJSONRecLensDec typeName = [d|instance FromJSON $a where parseJSON = genericParseJSON stripAllLensPrefixOptions|]
  where
    a = conT typeName

-- instance ToJSON RolloutInfo where toJSON = genericToJSON (stripAllLensPrefixOptions {omitNothingFields = True})
toJSONRecLensDec :: Name -> Q [Dec]
toJSONRecLensDec typeName =
  [d|instance ToJSON $a where toJSON = genericToJSON (stripAllLensPrefixOptions {omitNothingFields = True})|]
  where
    a = conT typeName

fromJSONRecUntaggedDec' :: Name -> Q [Dec]
fromJSONRecUntaggedDec' typeName = [d|instance FromJSON $a where parseJSON = genericParseJSON (stripAllLensPrefixOptions {sumEncoding = UntaggedValue})|]
  where
    a = conT typeName

fromJSONRecDec :: Name -> Q [Dec]
fromJSONRecDec typeName = [d|instance FromJSON $a where parseJSON = genericParseJSON (defaultOptions)|]
  where
    a = conT typeName

toJSONRecDec :: Name -> Q [Dec]
toJSONRecDec typeName =
  [d|instance ToJSON $a where toJSON = genericToJSON (defaultOptions)|]
  where
    a = conT typeName

toJSONRecUntaggedDec' :: Name -> Q [Dec]
toJSONRecUntaggedDec' typeName = [d|instance ToJSON $a where toJSON = genericToJSON (stripAllLensPrefixOptions {sumEncoding = UntaggedValue})|]
  where
    a = conT typeName

fromJSONRecUntaggedDec :: Name -> Q [Dec]
fromJSONRecUntaggedDec typeName = [d|instance FromJSON $a where parseJSON = genericParseJSON (defaultOptions {sumEncoding = UntaggedValue})|]
  where
    a = conT typeName

toJSONRecUntaggedDec :: Name -> Q [Dec]
toJSONRecUntaggedDec typeName = [d|instance ToJSON $a where toJSON = genericToJSON (defaultOptions {sumEncoding = UntaggedValue})|]
  where
    a = conT typeName

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be ReleaseAction where
--   sqlValueSyntax = autoSqlValueSyntax
hasSqlValueSyntaxInstanceDec :: Name -> Q [Dec]
hasSqlValueSyntaxInstanceDec typeName =
  [d|
    instance HasSqlValueSyntax $v String => HasSqlValueSyntax $v $a where
      sqlValueSyntax = autoSqlValueSyntax
    |]
  where
    a = conT typeName
    v = varT (mkName "db")

--instance HasSqlValueSyntax be Value => HasSqlValueSyntax be RolloutInfo where
-- sqlValueSyntax = compose1 sqlValueSyntax toJSON

hasSqlValueSyntaxRecInstanceDec :: Name -> Q [Dec]
hasSqlValueSyntaxRecInstanceDec typeName =
  [d|
    instance HasSqlValueSyntax $v Value => HasSqlValueSyntax $v $a where
      sqlValueSyntax = sqlValueSyntax . toJSON
    |]
  where
    a = conT typeName
    v = varT (mkName "db")

hasSqlValueSyntaxRecListInstanceDec :: Name -> Q [Dec]
hasSqlValueSyntaxRecListInstanceDec typeName =
  [d|
    instance HasSqlValueSyntax $v Value => HasSqlValueSyntax $v [$a] where
      sqlValueSyntax = sqlValueSyntax . toJSON
    |]
  where
    a = conT typeName
    v = varT (mkName "db")

--instance BeamBackend Postgres => FromBackendRow Postgres ReleaseInfo where

fromBackendRowDec :: Name -> Q [Dec]
fromBackendRowDec typeName =
  [d|instance BeamBackend Postgres => FromBackendRow Postgres $a|]
  where
    a = conT typeName

fromBackendRowListDec :: Name -> Q [Dec]
fromBackendRowListDec typeName =
  [d|instance BeamBackend Postgres => FromBackendRow Postgres [$a]|]
  where
    a = conT typeName

-- instance BeamSqlBackend be => HasSqlEqualityCheck be ReleaseAction
hasSqlEqualityCheckInstanceDec :: Name -> Q [Dec]
hasSqlEqualityCheckInstanceDec typeName =
  [d|instance BeamSqlBackend $v => HasSqlEqualityCheck $v $a|]
  where
    a = conT typeName
    v = varT (mkName "db")

-- instance FromField RolloutInfo where fromField = genericFromField "RolloutInfo"
deriveFromFieldRecDec :: Name -> Q [Dec]
deriveFromFieldRecDec typeName =
  [d|instance FromField $a where fromField = genericFromField $aString|]
  where
    a = conT typeName
    aString = litE (StringL $ nameBase typeName)

deriveFromFieldRecListDec :: Name -> Q [Dec]
deriveFromFieldRecListDec typeName =
  [d|instance FromField [$a] where fromField = genericFromField $aString|]
  where
    a = conT typeName
    aString = litE (StringL $ nameBase typeName)

-- deriving instance B.Beamable ReleaseTrackerT
beamableDec :: Name -> Q [Dec]
beamableDec typeName = [d|deriving instance Beamable $a|] where a = conT typeName

-- deriving instance Generic (ReleaseTrackerT f)
genericVarDec :: Name -> Q [Dec]
genericVarDec typeName =
  [d|deriving instance Generic ($a $v)|]
  where
    a = conT typeName
    v = varT (mkName "f")

--- Combined Functions

deriveCommonInstances :: Name -> Q [Dec]
deriveCommonInstances typeName = deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq, ''FromJSON, ''ToJSON]

deriveCommonInstancesMap :: [Name] -> Q [Dec]
deriveCommonInstancesMap = fmap concat . traverse deriveCommonInstances

deriveRecordLensInstances :: Name -> Q [Dec]
deriveRecordLensInstances typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq]
    +++ fromJSONRecLensDec typeName
    +++ toJSONRecLensDec typeName

deriveRecordLensInstancesMap :: [Name] -> Q [Dec]
deriveRecordLensInstancesMap = fmap concat . traverse deriveRecordLensInstances

deriveRecordInstances :: Name -> Q [Dec]
deriveRecordInstances typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq]
    +++ fromJSONRecDec typeName
    +++ toJSONRecDec typeName

deriveAllRecordInstances :: Name -> Q [Dec]
deriveAllRecordInstances typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq, ''Read]
    +++ fromJSONRecDec typeName
    +++ toJSONRecDec typeName

deriveRecordInstancesMap :: [Name] -> Q [Dec]
deriveRecordInstancesMap = fmap concat . traverse deriveRecordInstances

-- TODO Rename Untagged functions
deriveRecordInstancesUntagged :: Name -> Q [Dec]
deriveRecordInstancesUntagged typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq]
    +++ fromJSONRecUntaggedDec typeName
    +++ toJSONRecUntaggedDec typeName

deriveRecordLensInstancesUntagged :: Name -> Q [Dec]
deriveRecordLensInstancesUntagged typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq]
    +++ fromJSONRecUntaggedDec' typeName
    +++ toJSONRecUntaggedDec' typeName

deriveRecordInstancesUntaggedMap :: [Name] -> Q [Dec]
deriveRecordInstancesUntaggedMap = fmap concat . traverse deriveRecordInstancesUntagged

deriveRecordInstancesUntagged' :: Name -> Q [Dec]
deriveRecordInstancesUntagged' typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show]
    +++ fromJSONRecUntaggedDec typeName
    +++ toJSONRecUntaggedDec typeName

deriveRecordInstancesUntaggedMap' :: [Name] -> Q [Dec]
deriveRecordInstancesUntaggedMap' = fmap concat . traverse deriveRecordInstancesUntagged'

deriveDbRecordInstances :: Name -> Q [Dec]
deriveDbRecordInstances typeName =
  --deriveRecordLensInstances typeName
  deriveRecordLensInstancesUntagged typeName
    +++ hasSqlValueSyntaxRecInstanceDec typeName
    +++ fromBackendRowDec typeName
    +++ deriveFromFieldRecDec typeName

deriveDbRecordInstancesMap :: [Name] -> Q [Dec]
deriveDbRecordInstancesMap = fmap concat . traverse deriveDbRecordInstances

deriveDbRecordListInstances :: Name -> Q [Dec]
deriveDbRecordListInstances typeName =
  deriveRecordLensInstances typeName
    +++ hasSqlValueSyntaxRecListInstanceDec typeName
    +++ fromBackendRowListDec typeName
    +++ deriveFromFieldRecListDec typeName

deriveDbRecordListInstancesMap :: [Name] -> Q [Dec]
deriveDbRecordListInstancesMap = fmap concat . traverse deriveDbRecordListInstances

deriveDbEnumInstances :: Name -> Q [Dec]
deriveDbEnumInstances typeName =
  deriveMultipleSimpleInstances typeName [''Generic, ''Show, ''Eq, ''Read]
    +++ fromJSONRecUntaggedDec' typeName
    +++ toJSONRecUntaggedDec' typeName
    +++ fromBackendRowDec typeName
    +++ hasSqlValueSyntaxInstanceDec typeName
    +++ hasSqlEqualityCheckInstanceDec typeName
    +++ sequence [deriveFromFieldDec typeName]

deriveDbEnumInstancesMap :: [Name] -> Q [Dec]
deriveDbEnumInstancesMap = fmap concat . traverse deriveDbEnumInstances

deriveMainDbRecordInstances :: Name -> [Name] -> Q [Dec]
deriveMainDbRecordInstances typeName pkInfo =
  genericVarDec typeName
    +++ beamableDec typeName
    +++ sequence
      [ typeAliasDec typeName,
        typeAliasPkDec typeName,
        --, primaryKeyGenericDec typeName pkInfo
        primaryKeyGenericDec typeName pkInfo
      ]
    +++ deriveMultipleSimpleInstances (mkName (alias typeName)) [''Show, ''Eq]
    +++ fromJSONRecLensDec (mkName (alias typeName))
    +++ toJSONRecLensDec (mkName (alias typeName))

makeLensesMap :: [Name] -> Q [Dec]
makeLensesMap = fmap concat . traverse makeLenses

-- Older format - To be modified to use quotes and splices

deriveFromFieldDec :: Name -> Q Dec
deriveFromFieldDec typename = do
  v <- newName "v"
  v' <- newName "n"
  mbValue <- newName "mbValue"
  f <- newName "f"
  let nothingMatch = Match (ConP 'Nothing []) (NormalB (VarE 'returnError `AppE` ConE 'UnexpectedNull `AppE` VarE f `AppE` LitE (StringL ""))) []
      nothing2Case = Match (ConP 'Nothing []) (NormalB (VarE 'returnError `AppE` ConE 'ConversionFailed `AppE` VarE f `AppE` LitE (StringL $ "Could not 'read' value for '" <> nameBase typename <> "'"))) []
      just2Case = Match (ConP 'Just [VarP v]) (NormalB (AppE (VarE 'pure) (VarE v))) []
      justMatch = Match (ConP 'Just [VarP v']) (NormalB (CaseE (VarE 'readMaybe `AppE` (VarE 'BS.unpack `AppE` VarE v')) [nothing2Case, just2Case])) []
  let body = NormalB (CaseE (VarE mbValue) [nothingMatch, justMatch])
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''FromField) (ConT typename))
      [FunD 'fromField [Clause [VarP f, VarP mbValue] body []]]

alias :: Name -> String
alias name = take (length (nameBase name) - 1) (nameBase name)

-- type ReleaseTracker = ReleaseTrackerT Identity
typeAliasDec :: Name -> Q Dec
typeAliasDec typename = do
  pure $
    TySynD
      (mkName (alias typename))
      []
      (AppT (ConT typename) (ConT ''Identity))

-- type ReleaseTrackerPrimaryKey = B.PrimaryKey ReleaseTrackerT Identity
typeAliasPkDec :: Name -> Q Dec
typeAliasPkDec typename = do
  pure $
    TySynD
      (mkName (alias typename <> "PrimaryKey"))
      []
      (AppT (AppT (ConT ''PrimaryKey) (ConT typename)) (ConT ''Identity))

primaryKeyGenericDec :: Name -> [Name] -> Q Dec
primaryKeyGenericDec typename pKL = do
  let pkName = mkName (alias typename <> "PrimaryKey")
  f <- newName "f"
  pkType <- traverse reify pKL
  let names = extractPKeyType <$> pkType
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Table) (ConT typename))
      [ DataInstD
          []
          Nothing
          (AppT (AppT (ConT ''PrimaryKey) (ConT typename)) (VarT f))
          Nothing
          [NormalC pkName $ map (genPKeyDef f) names]
          [DerivClause Nothing [ConT ''Generic, ConT ''Beamable]],
        ValD (VarP 'primaryKey) (NormalB $ genPKeyImpl pkName (genPKeyOp pKL)) []
      ]
  where
    genPKeyDef f typeDec = (Bang NoSourceUnpackedness NoSourceStrictness, AppT (AppT (ConT ''C) (VarT f)) (ConT typeDec))
    genPKeyImpl pkName v = foldl' (\b a -> (InfixE (Just b) (VarE $ snd a) (Just (VarE (fst a))))) (ConE pkName) v
    genPKeyOp (x : xs) = (x, '(<$>)) : map (\x' -> (x', '(<*>))) xs
    genPKeyOp [] = []

extractPKeyType :: Info -> Name
extractPKeyType (VarI _ (ForallT _ _ (AppT _ (AppT _ (ConT x)))) _) = x
extractPKeyType _ = mkName "_"
