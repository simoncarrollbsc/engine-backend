module Registry.Service.PackageBundle.PackageBundleService
  ( getPackageBundle
  ) where

import Control.Lens ((^.))

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageService
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Shared.Constant.KnowledgeModel
import Shared.Model.PackageBundle.PackageBundle
import Shared.Service.PackageBundle.PackageBundleMapper

getPackageBundle :: String -> AppContextM PackageBundleDTO
getPackageBundle pbId = do
  _ <- auditGetPackageBundle pbId
  packages <- getSeriesOfPackages pbId
  let newestPackage = last packages
  let pb =
        PackageBundle
          { _bundleId = newestPackage ^. pId
          , _name = newestPackage ^. name
          , _organizationId = newestPackage ^. organizationId
          , _kmId = newestPackage ^. kmId
          , _version = newestPackage ^. version
          , _metamodelVersion = kmMetamodelVersion
          , _packages = packages
          }
  return . toDTO $ pb
