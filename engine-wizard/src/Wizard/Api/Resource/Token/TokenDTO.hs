module Wizard.Api.Resource.Token.TokenDTO where

import GHC.Generics

data TokenDTO =
  TokenDTO
    { _token :: String
    }
  deriving (Show, Eq, Generic)
