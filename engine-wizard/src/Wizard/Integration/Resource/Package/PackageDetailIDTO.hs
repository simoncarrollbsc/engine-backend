module Wizard.Integration.Resource.Package.PackageDetailIDTO where

import Data.Time

import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO

data PackageDetailIDTO =
  PackageDetailIDTO
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
    , _organization :: OrganizationSimpleIDTO
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq)
