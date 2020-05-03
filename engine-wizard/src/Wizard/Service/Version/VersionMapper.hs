module Wizard.Service.Version.VersionMapper where

import Control.Lens ((&), (.~), (^.))
import Data.Time

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Config.AppConfig
import Wizard.Service.Package.PackageUtils

fromBranch :: BranchWithEvents -> PackageWithEvents -> BranchWithEvents
fromBranch branch pkg = (branch & events .~ []) & previousPackageId .~ (Just $ pkg ^. pId)

fromPackage ::
     BranchWithEvents
  -> VersionDTO
  -> Maybe String
  -> Maybe String
  -> AppConfigOrganization
  -> String
  -> [Event]
  -> UTCTime
  -> PackageWithEvents
fromPackage branch versionDto forkOfPkgId mergeCheckpointPkgId org version events now =
  PackageWithEvents
    { _pId = buildPackageId (org ^. organizationId) (branch ^. kmId) version
    , _name = branch ^. name
    , _organizationId = org ^. organizationId
    , _kmId = branch ^. kmId
    , _version = version
    , _metamodelVersion = kmMetamodelVersion
    , _description = versionDto ^. description
    , _readme = versionDto ^. readme
    , _license = versionDto ^. license
    , _previousPackageId = branch ^. previousPackageId
    , _forkOfPackageId = forkOfPkgId
    , _mergeCheckpointPackageId = mergeCheckpointPkgId
    , _events = events
    , _createdAt = now
    }
