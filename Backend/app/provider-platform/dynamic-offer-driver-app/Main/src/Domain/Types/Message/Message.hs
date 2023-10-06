{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Message.Message where

import Data.Map as HM
import qualified Domain.Types.MediaFile as MF
import Domain.Types.Merchant (Merchant)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Tools.Beam.UtilsTH as TH

data MessageType = Action Text | Read
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(TH.mkBeamInstancesForEnum ''MessageType)

data Message = Message
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    likeCount :: Int,
    viewCount :: Int,
    mediaFiles :: [Id MF.MediaFile],
    messageTranslations :: [MessageTranslation],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    createdAt :: UTCTime
  }

data RawMessage = RawMessage
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    likeCount :: Int,
    viewCount :: Int,
    mediaFiles :: [Id MF.MediaFile],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

data MessageDict = MessageDict
  { defaultMessage :: RawMessage,
    translations :: HM.Map Text RawMessage
  }
  deriving (Generic, ToJSON, FromJSON)
