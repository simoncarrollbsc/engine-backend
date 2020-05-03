module Registry.Api.Resource.Organization.OrganizationChangeDTO where

import GHC.Generics

data OrganizationChangeDTO =
  OrganizationChangeDTO
    { _name :: String
    , _description :: String
    , _email :: String
    }
  deriving (Show, Eq, Generic)
