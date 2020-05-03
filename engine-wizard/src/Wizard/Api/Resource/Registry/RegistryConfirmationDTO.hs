module Wizard.Api.Resource.Registry.RegistryConfirmationDTO where

import GHC.Generics

data RegistryConfirmationDTO =
  RegistryConfirmationDTO
    { _organizationId :: String
    , _hash :: String
    }
  deriving (Show, Eq, Generic)
