module Types.API.Feedback
  ( FeedbackReq,
    FeedbackRes,
  )
where

import Beckn.Types.Common (AckResponse)
import EulerHS.Prelude

data FeedbackReq = FeedbackReq
  { caseId :: Text,
    productInstanceId :: Text,
    rating :: Text -- ENUM maybe?
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type FeedbackRes = AckResponse
