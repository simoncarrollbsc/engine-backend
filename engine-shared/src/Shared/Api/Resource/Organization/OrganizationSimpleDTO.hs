module Shared.Api.Resource.Organization.OrganizationSimpleDTO where

import GHC.Generics

data OrganizationSimpleDTO =
  OrganizationSimpleDTO
    { _organizationId :: String
    , _name :: String
    , _logo :: Maybe String
    }
  deriving (Show, Eq, Generic)
