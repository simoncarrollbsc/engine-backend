module Wizard.Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Template.Template

data TemplateDTO =
  TemplateDTO
    { _uuid :: U.UUID
    , _name :: String
    , _description :: String
    , _allowedPackages :: [PackageSimpleDTO]
    , _recommendedPackageId :: Maybe String
    , _formats :: [TemplateFormat]
    }
  deriving (Show, Eq, Generic)
