module Wizard.Api.Resource.Submission.SubmissionCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data SubmissionCreateDTO =
  SubmissionCreateDTO
    { _serviceId :: String
    , _docUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
