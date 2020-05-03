module Wizard.Api.Resource.Typehint.TypehintDTO where

import GHC.Generics

data TypehintDTO =
  TypehintDTO
    { _intId :: String
    , _name :: String
    , _url :: String
    }
  deriving (Show, Eq, Generic)
