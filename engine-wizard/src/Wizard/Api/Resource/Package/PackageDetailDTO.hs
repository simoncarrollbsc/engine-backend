module Wizard.Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Model.Package.PackageState

data PackageDetailDTO =
  PackageDetailDTO
    { _pId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _description :: String
    , _readme :: String
    , _license :: String
    , _metamodelVersion :: Int
    , _previousPackageId :: Maybe String
    , _forkOfPackageId :: Maybe String
    , _mergeCheckpointPackageId :: Maybe String
    , _versions :: [String]
    , _remoteLatestVersion :: Maybe String
    , _organization :: Maybe OrganizationSimpleDTO
    , _registryLink :: Maybe String
    , _state :: PackageState
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
