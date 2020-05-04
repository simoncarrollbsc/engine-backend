module Wizard.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Service.Package.PackageUtils

toSimpleDTO :: Package -> PackageSimpleDTO
toSimpleDTO pkg = toSimpleDTO' pkg [] [] []

toSimpleDTO' :: Package -> [PackageSimpleIDTO] -> [OrganizationSimpleDTO] -> [String] -> PackageSimpleDTO
toSimpleDTO' pkg pkgRs orgRs localVersions =
  PackageSimpleDTO
    { _pId = pkg ^. pId
    , _name = pkg ^. name
    , _organizationId = pkg ^. organizationId
    , _kmId = pkg ^. kmId
    , _version = pkg ^. version
    , _versions = localVersions
    , _description = pkg ^. description
    , _state = computePackageState pkgRs pkg
    , _organization = selectOrganizationByOrgId pkg orgRs
    , _createdAt = pkg ^. createdAt
    }

toDetailDTO :: Package -> [PackageSimpleIDTO] -> [OrganizationSimpleDTO] -> [String] -> String -> PackageDetailDTO
toDetailDTO pkg pkgRs orgRs versionLs registryLink =
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
    , _versions = versionLs
    , _remoteLatestVersion =
        case selectPackageByOrgIdAndKmId pkg pkgRs of
          Just pkgR -> Just $ pkgR ^. version
          Nothing -> Nothing
    , _state = computePackageState pkgRs pkg
    , _registryLink =
        case selectPackageByOrgIdAndKmId pkg pkgRs of
          Just pkgR -> Just registryLink
          Nothing -> Nothing
    , _organization = selectOrganizationByOrgId pkg orgRs
    , _createdAt = pkg ^. createdAt
    }

fromDTO :: PackageDTO -> PackageWithEvents
fromDTO dto =
  PackageWithEvents
    { _pId = dto ^. pId
    , _name = dto ^. name
    , _organizationId = dto ^. organizationId
    , _kmId = dto ^. kmId
    , _version = dto ^. version
    , _metamodelVersion = dto ^. metamodelVersion
    , _description = dto ^. description
    , _readme = dto ^. readme
    , _license = dto ^. license
    , _previousPackageId = dto ^. previousPackageId
    , _forkOfPackageId = dto ^. forkOfPackageId
    , _mergeCheckpointPackageId = dto ^. mergeCheckpointPackageId
    , _events = fromDTOs (dto ^. events)
    , _createdAt = dto ^. createdAt
    }

buildPackageUrl :: String -> String -> String
buildPackageUrl clientRegistryUrl pkgId = clientRegistryUrl ++ "/knowledge-models/" ++ pkgId
