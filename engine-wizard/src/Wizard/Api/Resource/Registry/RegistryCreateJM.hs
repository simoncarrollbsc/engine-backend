module Wizard.Api.Resource.Registry.RegistryCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Registry.RegistryCreateDTO

instance FromJSON RegistryCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON RegistryCreateDTO where
  toJSON = genericToJSON simpleOptions
