module Registry.Model.Config.ServerConfigJM where

import Data.Aeson

import Registry.Model.Config.ServerConfig
import Shared.Model.Config.EnvironmentJM ()
import Shared.Util.JSON

instance FromJSON ServerConfig where
  parseJSON = genericParseJSON simpleOptions'''

instance FromJSON ServerConfigGeneral where
  parseJSON = genericParseJSON simpleOptions'''

instance FromJSON ServerConfigDatabase where
  parseJSON = genericParseJSON simpleOptions'''

instance FromJSON ServerConfigMail where
  parseJSON = genericParseJSON simpleOptions'''

instance FromJSON ServerConfigAnalytics where
  parseJSON = genericParseJSON simpleOptions'''
