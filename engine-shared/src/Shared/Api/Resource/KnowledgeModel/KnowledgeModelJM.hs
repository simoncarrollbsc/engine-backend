module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.JSON

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON KnowledgeModel where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON KnowledgeModel where
  toJSON = genericToJSON simpleOptions

instance FromJSON KnowledgeModelEntities where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON KnowledgeModelEntities where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Chapter where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Chapter where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

instance FromJSON Question where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Question where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Answer where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Answer where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Expert where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Expert where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Reference where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Reference where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Metric where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Metric where
  toJSON = genericToJSON simpleOptions

instance FromJSON MetricMeasure where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MetricMeasure where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Tag where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Tag where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON Integration where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Integration where
  toJSON = genericToJSON simpleOptions