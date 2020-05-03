module Wizard.Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO

globalRemotePackage :: PackageSimpleIDTO
globalRemotePackage =
  PackageSimpleIDTO
    { _pId = globalPackage ^. pId
    , _name = globalPackage ^. name
    , _organizationId = globalPackage ^. organizationId
    , _kmId = globalPackage ^. kmId
    , _version = globalPackage ^. version
    , _description = globalPackage ^. description
    , _organization =
        OrganizationSimpleIDTO
          { _organizationSimpleIDTOOrganizationId = orgGlobalSimple ^. organizationId
          , _organizationSimpleIDTOName = orgGlobalSimple ^. name
          , _organizationSimpleIDTOLogo = Just orgLogo
          }
    , _createdAt = globalPackage ^. createdAt
    }

globalNetherlandsPackage :: PackageSimpleIDTO
globalNetherlandsPackage =
  PackageSimpleIDTO
    { _pId = netherlandsPackageV2 ^. pId
    , _name = netherlandsPackageV2 ^. name
    , _organizationId = netherlandsPackageV2 ^. organizationId
    , _kmId = netherlandsPackageV2 ^. kmId
    , _version = netherlandsPackageV2 ^. version
    , _description = netherlandsPackageV2 ^. description
    , _organization =
        OrganizationSimpleIDTO
          { _organizationSimpleIDTOOrganizationId = orgNetherlandsSimple ^. organizationId
          , _organizationSimpleIDTOName = orgNetherlandsSimple ^. name
          , _organizationSimpleIDTOLogo = Just orgLogo
          }
    , _createdAt = globalPackage ^. createdAt
    }
