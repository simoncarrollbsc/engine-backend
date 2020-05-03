module Wizard.Service.Template.TemplateMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Package.Package
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Template.Template
import Wizard.Service.Package.PackageMapper

toDTO :: [Package] -> Template -> TemplateDTO
toDTO pkgs template =
  TemplateDTO
    { _uuid = template ^. uuid
    , _name = template ^. name
    , _description = template ^. description
    , _allowedPackages = fmap toSimpleDTO pkgs
    , _recommendedPackageId = template ^. recommendedPackageId
    , _formats = template ^. formats
    }
