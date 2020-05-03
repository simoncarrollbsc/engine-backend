module Wizard.Api.Resource.Registry.RegistryCreateDTO where

import GHC.Generics

data RegistryCreateDTO =
  RegistryCreateDTO
    { _email :: String
    }
  deriving (Show, Eq, Generic)
