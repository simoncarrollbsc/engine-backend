module Wizard.Api.Resource.Submission.SubmissionDTO where

import GHC.Generics

data SubmissionDTO =
  SubmissionDTO
    { _location :: Maybe String
    }
  deriving (Show, Eq, Generic)
