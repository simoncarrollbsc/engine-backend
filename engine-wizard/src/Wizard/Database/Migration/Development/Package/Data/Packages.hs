module Wizard.Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO

globalRemotePackage :: PackageSimpleIDTO
globalRemotePackage =
  PackageSimpleIDTO
    { _packageSimpleIDTOPId = globalPackage ^. pId
    , _packageSimpleIDTOName = globalPackage ^. name
    , _packageSimpleIDTOOrganizationId = globalPackage ^. organizationId
    , _packageSimpleIDTOKmId = globalPackage ^. kmId
    , _packageSimpleIDTOVersion = globalPackage ^. version
    , _packageSimpleIDTODescription = globalPackage ^. description
    , _packageSimpleIDTOOrganization =
        OrganizationSimpleIDTO
          { _organizationSimpleIDTOOrganizationId = orgGlobal ^. organizationId
          , _organizationSimpleIDTOName = orgGlobal ^. name
          , _organizationSimpleIDTOLogo = Just orgLogo
          }
    , _packageSimpleIDTOCreatedAt = globalPackage ^. createdAt
    }

globalNetherlandsPackage :: PackageSimpleIDTO
globalNetherlandsPackage =
  PackageSimpleIDTO
    { _packageSimpleIDTOPId = netherlandsPackageV2 ^. pId
    , _packageSimpleIDTOName = netherlandsPackageV2 ^. name
    , _packageSimpleIDTOOrganizationId = netherlandsPackageV2 ^. organizationId
    , _packageSimpleIDTOKmId = netherlandsPackageV2 ^. kmId
    , _packageSimpleIDTOVersion = netherlandsPackageV2 ^. version
    , _packageSimpleIDTODescription = netherlandsPackageV2 ^. description
    , _packageSimpleIDTOOrganization =
        OrganizationSimpleIDTO
          { _organizationSimpleIDTOOrganizationId = orgNetherlands ^. organizationId
          , _organizationSimpleIDTOName = orgNetherlands ^. name
          , _organizationSimpleIDTOLogo = Just orgLogo
          }
    , _packageSimpleIDTOCreatedAt = globalPackage ^. createdAt
    }

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
