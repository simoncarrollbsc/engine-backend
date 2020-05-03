module Wizard.Model.Template.Template where

import qualified Data.UUID as U
import GHC.Generics

data Template =
  Template
    { _uuid :: U.UUID
    , _name :: String
    , _description :: String
    , _allowedPackages :: [TemplateAllowedPackage]
    , _recommendedPackageId :: Maybe String
    , _formats :: [TemplateFormat]
    }
  deriving (Show, Eq, Generic)

data TemplateAllowedPackage =
  TemplateAllowedPackage
    { _orgId :: Maybe String
    , _kmId :: Maybe String
    , _minVersion :: Maybe String
    , _maxVersion :: Maybe String
    }
  deriving (Show, Eq, Generic)

data TemplateFormat =
  TemplateFormat
    { _uuid :: U.UUID
    , _name :: String
    , _shortName :: String
    , _icon :: String
    , _color :: String
    }
  deriving (Show, Eq, Generic)
