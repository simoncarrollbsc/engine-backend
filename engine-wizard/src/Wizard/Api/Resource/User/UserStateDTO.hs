module Wizard.Api.Resource.User.UserStateDTO where

import GHC.Generics

data UserStateDTO =
  UserStateDTO
    { _active :: Bool
    }
  deriving (Generic)
