module Wizard.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User

data UserProfileChangeDTO =
  UserProfileChangeDTO
    { _firstName :: String
    , _lastName :: String
    , _email :: Email
    , _affiliation :: Maybe String
    , _submissionProps :: [UserSubmissionPropsDTO]
    }
  deriving (Generic)
