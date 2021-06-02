{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Geometry as Geometry
import qualified Beckn.Types.Storage.Issue as Issue
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)

data AppDb f = AppDb
  { organization :: f (B.TableEntity Organization.OrganizationT),
    issues :: f (B.TableEntity Issue.IssueT),
    location :: f (B.TableEntity Location.LocationT),
    person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    productInstance :: f (B.TableEntity ProductInstance.ProductInstanceT),
    products :: f (B.TableEntity Products.ProductsT),
    registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    geometry :: f (B.TableEntity Geometry.GeometryT)
  }
  deriving (Generic, B.Database be)

appDb :: Text -> B.DatabaseSettings be AppDb
appDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { organization = setSchema dbSchemaName <> Organization.fieldEMod,
        issues = setSchema dbSchemaName <> Issue.fieldEMod,
        location = setSchema dbSchemaName <> Location.fieldEMod,
        person = setSchema dbSchemaName <> Person.fieldEMod,
        _case = setSchema dbSchemaName <> Case.fieldEMod,
        productInstance = setSchema dbSchemaName <> ProductInstance.fieldEMod,
        products = setSchema dbSchemaName <> Products.fieldEMod,
        registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod,
        geometry = setSchema dbSchemaName <> Geometry.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
