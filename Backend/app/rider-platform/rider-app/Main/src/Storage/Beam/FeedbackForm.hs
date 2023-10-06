{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.FeedbackForm where

import qualified Database.Beam as B
import qualified Domain.Types.FeedbackForm as Domain
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data FeedbackFormT f = FeedbackFormT
  { id :: B.C f Text,
    categoryName :: B.C f Domain.Category,
    rating :: B.C f (Maybe Int),
    question :: B.C f Text,
    answer :: B.C f [Text],
    answerType :: B.C f Domain.AnswerType
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackFormT where
  data PrimaryKey FeedbackFormT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FeedbackForm = FeedbackFormT Identity

$(TH.enableKVPG ''FeedbackFormT ['id] [])

$(TH.mkTableInstances ''FeedbackFormT "feedback_form")
