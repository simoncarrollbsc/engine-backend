module Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO where

import GHC.Generics

data SubmissionServiceSimpleDTO =
  SubmissionServiceSimpleDTO
    { _id :: String
    , _name :: String
    , _description :: String
    }
  deriving (Show, Eq, Generic)
