module Wizard.Integration.Resource.Package.PackageSimpleIDTO where

import Data.Time

import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO

data PackageSimpleIDTO =
  PackageSimpleIDTO
    { _pId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _description :: String
    , _organization :: OrganizationSimpleIDTO
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq)
