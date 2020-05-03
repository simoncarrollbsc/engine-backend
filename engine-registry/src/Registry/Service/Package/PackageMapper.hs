module Registry.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Model.Organization.Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM
import Shared.Model.Package.Package

toSimpleDTO :: Package -> Organization -> PackageSimpleDTO
toSimpleDTO pkg org =
  PackageSimpleDTO
    { _pId = pkg ^. pId
    , _name = pkg ^. name
    , _organizationId = pkg ^. organizationId
    , _kmId = pkg ^. kmId
    , _version = pkg ^. version
    , _description = pkg ^. description
    , _createdAt = pkg ^. createdAt
    , _organization = OM.toSimpleDTO org
    }

toDetailDTO :: Package -> [String] -> Organization -> PackageDetailDTO
toDetailDTO pkg versions org =
  PackageDetailDTO
    { _pId = pkg ^. pId
    , _name = pkg ^. name
    , _organizationId = pkg ^. organizationId
    , _kmId = pkg ^. kmId
    , _version = pkg ^. version
    , _description = pkg ^. description
    , _readme = pkg ^. readme
    , _license = pkg ^. license
    , _metamodelVersion = pkg ^. metamodelVersion
    , _previousPackageId = pkg ^. previousPackageId
    , _forkOfPackageId = pkg ^. forkOfPackageId
    , _mergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _versions = versions
    , _organization = OM.toSimpleDTO org
    , _createdAt = pkg ^. createdAt
    }
