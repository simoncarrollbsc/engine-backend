module Wizard.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Wizard.Model.ActionKey.ActionKey

data ActionKeyDTO =
  ActionKeyDTO
    { _aType :: ActionKeyType
    , _email :: String
    }
  deriving (Show, Eq, Generic)
