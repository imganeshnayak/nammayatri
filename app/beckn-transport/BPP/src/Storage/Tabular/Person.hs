{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Person where

import Beckn.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Centesimal
import Beckn.Types.Id
import qualified Domain.Types.Person as Domain
import Storage.Tabular.Organization (OrganizationTId)

derivePersistField "Domain.Role"
derivePersistField "Domain.Gender"
derivePersistField "Domain.IdentifierType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      firstName Text
      middleName Text Maybe
      lastName Text Maybe
      role Domain.Role
      gender Domain.Gender
      identifierType Domain.IdentifierType
      email Text Maybe
      mobileNumberEncrypted Text Maybe
      mobileNumberHash DbHash Maybe
      mobileCountryCode Text Maybe
      passwordHash DbHash Maybe
      identifier Text Maybe
      rating Centesimal Maybe
      isNew Bool
      organizationId OrganizationTId Maybe
      deviceToken FCMRecipientToken Maybe
      description Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey (Id id) = PersonTKey id

instance TType PersonT Domain.Person where
  fromTType PersonT {..} = do
    return $
      Domain.Person
        { id = Id id,
          mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
          organizationId = fromKey <$> organizationId,
          ..
        }
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber <&> (.hash),
        organizationId = toKey <$> organizationId,
        ..
      }
