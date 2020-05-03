module Shared.Api.Resource.PackageBundle.PackageBundleDTO where

import GHC.Generics
import Shared.Api.Resource.Package.PackageDTO

data PackageBundleDTO =
  PackageBundleDTO
    { _bundleId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _metamodelVersion :: Int
    , _packages :: [PackageDTO]
    }
  deriving (Show, Eq, Generic)
