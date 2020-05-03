module Wizard.Api.Resource.User.UserChangeDTO where

import Data.UUID
import GHC.Generics

import Wizard.Model.User.User

data UserChangeDTO =
  UserChangeDTO
    { _uuid :: UUID
    , _firstName :: String
    , _lastName :: String
    , _email :: Email
    , _affiliation :: Maybe String
    , _role :: Role
    , _active :: Bool
    }
  deriving (Generic)
