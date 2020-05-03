module Shared.Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.PackageBundle.PackageBundle
import qualified Shared.Service.Package.PackageMapper as PM

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { _bundleId = pb ^. bundleId
    , _name = pb ^. name
    , _organizationId = pb ^. organizationId
    , _kmId = pb ^. kmId
    , _version = pb ^. version
    , _metamodelVersion = pb ^. metamodelVersion
    , _packages = PM.toDTO <$> pb ^. packages
    }
