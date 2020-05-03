module Shared.Model.Package.Package where

import Data.Time
import GHC.Generics

data Package =
  Package
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
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Ord Package where
  compare a b =
    (compare (_organizationId a) (_organizationId b)) <> (compare (_kmId a) (_kmId b)) <>
    (compare (_version a) (_version b))
