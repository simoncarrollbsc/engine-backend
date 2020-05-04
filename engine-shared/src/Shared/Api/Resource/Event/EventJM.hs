module Shared.Api.Resource.Event.EventJM where

import Data.Aeson

import Shared.Model.Event.Event
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON

instance FromJSON Event where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON Event where
  toJSON = genericToJSON simpleOptions'''