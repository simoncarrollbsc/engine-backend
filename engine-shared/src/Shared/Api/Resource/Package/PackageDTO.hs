module Shared.Api.Resource.Package.PackageDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data PackageDTO =
  PackageDTO
    { _pId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _metamodelVersion :: Int
    , _description :: String
    , _readme :: String
    , _license :: String
    , _previousPackageId :: Maybe String
    , _forkOfPackageId :: Maybe String
    , _mergeCheckpointPackageId :: Maybe String
    , _events :: [EventDTO]
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
