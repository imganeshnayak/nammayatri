module Types.API.OrgAdmin where

import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Domain.Types.Organization (OrganizationAPIEntity)
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)

data OrgAdminProfileRes = OrgAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: OrganizationAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOrgAdminProfileReq = UpdateOrgAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type UpdateOrgAdminProfileRes = OrgAdminProfileRes
