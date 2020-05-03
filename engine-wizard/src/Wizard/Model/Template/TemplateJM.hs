module Wizard.Model.Template.TemplateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Template.Template

instance FromJSON Template where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON Template where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON TemplateAllowedPackage where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON TemplateAllowedPackage where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON TemplateFormat where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON TemplateFormat where
  toJSON = genericToJSON simpleOptions'''
