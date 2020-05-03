module Shared.Database.Migration.Development.PackageBundle.Data.PackageBundles where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.PackageBundle.PackageBundle

netherlandsPackageV2Budle :: PackageBundle
netherlandsPackageV2Budle =
  PackageBundle
    { _bundleId = netherlandsPackageV2 ^. pId
    , _name = netherlandsPackageV2 ^. name
    , _organizationId = netherlandsPackageV2 ^. organizationId
    , _kmId = netherlandsPackageV2 ^. kmId
    , _version = netherlandsPackageV2 ^. version
    , _metamodelVersion = netherlandsPackageV2 ^. metamodelVersion
    , _packages = [globalPackage, netherlandsPackage, netherlandsPackageV2]
    }

germanyBundle :: PackageBundle
germanyBundle =
  PackageBundle
    { _bundleId = germanyPackage ^. pId
    , _name = germanyPackage ^. name
    , _organizationId = germanyPackage ^. organizationId
    , _kmId = germanyPackage ^. kmId
    , _version = germanyPackage ^. version
    , _metamodelVersion = germanyPackage ^. metamodelVersion
    , _packages = [globalPackageEmpty, germanyPackage]
    }
