module Wizard.Api.Resource.Config.SimpleFeatureJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Config.SimpleFeature

instance FromJSON SimpleFeature where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SimpleFeature where
  toJSON = genericToJSON simpleOptions
