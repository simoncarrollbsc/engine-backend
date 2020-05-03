module Registry.Api.Resource.Organization.OrganizationStateDTO where

import GHC.Generics

data OrganizationStateDTO =
  OrganizationStateDTO
    { _active :: Bool
    }
  deriving (Show, Eq, Generic)
