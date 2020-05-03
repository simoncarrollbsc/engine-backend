module Registry.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

data PackageSimpleDTO =
  PackageSimpleDTO
    { _pId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _description :: String
    , _organization :: OrganizationSimpleDTO
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
