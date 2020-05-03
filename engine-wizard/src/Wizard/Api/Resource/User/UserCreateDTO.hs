module Wizard.Api.Resource.User.UserCreateDTO where

import GHC.Generics

import Wizard.Model.User.User

data UserCreateDTO =
  UserCreateDTO
    { _firstName :: String
    , _lastName :: String
    , _email :: Email
    , _affiliation :: Maybe String
    , _role :: Maybe Role
    , _password :: String
    }
  deriving (Generic)
