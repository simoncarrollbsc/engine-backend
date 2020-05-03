module Wizard.Api.Resource.Token.TokenCreateDTO where

import GHC.Generics

data TokenCreateDTO =
  TokenCreateDTO
    { _email :: String
    , _password :: String
    }
  deriving (Generic)
