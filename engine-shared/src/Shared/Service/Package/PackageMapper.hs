module Shared.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Event.EventMapper

toPackage :: PackageWithEvents -> Package
toPackage pkg =
  Package
    { _pId = pkg ^. pId
    , _name = pkg ^. name
    , _organizationId = pkg ^. organizationId
    , _kmId = pkg ^. kmId
    , _version = pkg ^. version
    , _metamodelVersion = pkg ^. metamodelVersion
    , _description = pkg ^. description
    , _readme = pkg ^. readme
    , _license = pkg ^. license
    , _previousPackageId = pkg ^. previousPackageId
    , _forkOfPackageId = pkg ^. forkOfPackageId
    , _mergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _createdAt = pkg ^. createdAt
    }

toDTO :: PackageWithEvents -> PackageDTO
toDTO pkg =
  PackageDTO
    { _pId = pkg ^. pId
    , _name = pkg ^. name
    , _organizationId = pkg ^. organizationId
    , _kmId = pkg ^. kmId
    , _version = pkg ^. version
    , _metamodelVersion = pkg ^. metamodelVersion
    , _description = pkg ^. description
    , _readme = pkg ^. readme
    , _license = pkg ^. license
    , _previousPackageId = pkg ^. previousPackageId
    , _forkOfPackageId = pkg ^. forkOfPackageId
    , _mergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _events = toDTOs (pkg ^. events)
    , _createdAt = pkg ^. createdAt
    }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion
