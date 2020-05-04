module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.JSON

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModel where
  toJSON = simpleToJSON "_knowledgeModel"

instance FromJSON KnowledgeModel where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON KnowledgeModelEntities where
  toJSON = simpleToJSON "_knowledgeModelEntities"

instance FromJSON KnowledgeModelEntities where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Chapter where
  toJSON = simpleToJSON "_chapter"

instance FromJSON Chapter where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

instance ToJSON Question where
  toJSON = toSumJSON

instance FromJSON Question where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (OptionsQuestion' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (ListQuestion' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (ValueQuestion' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (IntegrationQuestion' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON OptionsQuestion where
  toJSON = simpleToJSON' "_optionsQuestion" "questionType"

instance FromJSON OptionsQuestion where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON ListQuestion where
  toJSON = simpleToJSON' "_listQuestion" "questionType"

instance FromJSON ListQuestion where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON ValueQuestion where
  toJSON = simpleToJSON' "_valueQuestion" "questionType"

instance FromJSON ValueQuestion where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestion where
  toJSON = simpleToJSON' "_integrationQuestion" "questionType"

instance FromJSON IntegrationQuestion where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Answer where
  toJSON = simpleToJSON "_answer"

instance FromJSON Answer where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Expert where
  toJSON = simpleToJSON "_expert"

instance FromJSON Expert where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Reference where
  toJSON = toSumJSON

instance FromJSON Reference where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (ResourcePageReference' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (URLReference' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (CrossReference' event)
      _ -> fail "One of the references has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ResourcePageReference where
  toJSON = simpleToJSON' "_resourcePageReference" "referenceType"

instance FromJSON ResourcePageReference where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON URLReference where
  toJSON = simpleToJSON' "_uRLReference" "referenceType"

instance FromJSON URLReference where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON CrossReference where
  toJSON = simpleToJSON' "_crossReference" "referenceType"

instance FromJSON CrossReference where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Metric where
  toJSON = genericToJSON simpleOptions

instance FromJSON Metric where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON MetricMeasure where
  toJSON = genericToJSON simpleOptions

instance FromJSON MetricMeasure where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Tag where
  toJSON = genericToJSON simpleOptions

instance FromJSON Tag where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Integration where
  toJSON = genericToJSON simpleOptions

instance FromJSON Integration where
  parseJSON = genericParseJSON simpleOptions
