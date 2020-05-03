module Wizard.Messaging.Resource.Document.DocumentCreateMJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Messaging.Resource.Document.DocumentCreateMDTO

instance FromJSON DocumentCreateMDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON DocumentCreateMDTO where
  toJSON = genericToJSON simpleOptions'''
