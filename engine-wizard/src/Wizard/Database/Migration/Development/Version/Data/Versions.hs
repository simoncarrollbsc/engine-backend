module Wizard.Database.Migration.Development.Version.Data.Versions where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Version.VersionDTO

versionAmsterdam :: VersionDTO
versionAmsterdam =
  VersionDTO
    { _description = amsterdamPackage ^. description
    , _readme = amsterdamPackage ^. readme
    , _license = amsterdamPackage ^. license
    }
