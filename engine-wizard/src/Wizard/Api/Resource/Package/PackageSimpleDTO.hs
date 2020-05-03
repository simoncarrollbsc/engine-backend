module Wizard.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Model.Package.PackageState

data PackageSimpleDTO =
  PackageSimpleDTO
    { _pId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _versions :: [String]
    , _description :: String
    , _state :: PackageState
    , _organization :: Maybe OrganizationSimpleDTO
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
