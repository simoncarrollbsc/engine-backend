module Registry.Api.Resource.Organization.OrganizationCreateDTO where

import GHC.Generics

data OrganizationCreateDTO =
  OrganizationCreateDTO
    { _organizationId :: String
    , _name :: String
    , _description :: String
    , _email :: String
    }
  deriving (Show, Eq, Generic)
