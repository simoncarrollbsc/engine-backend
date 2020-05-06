module Wizard.Api.Resource.Feedback.FeedbackCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data FeedbackCreateDTO =
  FeedbackCreateDTO
    { _questionUuid :: U.UUID
    , _packageId :: String
    , _title :: String
    , _content :: String
    }
  deriving (Show, Eq, Generic)
