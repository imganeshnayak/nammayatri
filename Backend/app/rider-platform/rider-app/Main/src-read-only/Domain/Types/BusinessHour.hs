{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.BusinessHour where

import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Tools.Beam.UtilsTH

data BusinessHour = BusinessHour
  { btype :: Domain.Types.BusinessHour.BusinessHourType,
    categoryId :: [Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory],
    id :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour
  }
  deriving (Generic, Show)

data BusinessHourType = Slot | Duration
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BusinessHourType)
