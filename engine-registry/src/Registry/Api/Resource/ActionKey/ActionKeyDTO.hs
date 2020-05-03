module Registry.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Registry.Model.ActionKey.ActionKey

data ActionKeyDTO =
  ActionKeyDTO
    { _aType :: ActionKeyType
    , _email :: String
    }
  deriving (Show, Eq, Generic)
