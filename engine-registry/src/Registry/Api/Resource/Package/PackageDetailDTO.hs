module Registry.Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

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
    , _organization :: OrganizationSimpleDTO
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
