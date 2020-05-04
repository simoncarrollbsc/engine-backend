module Shared.Api.Resource.Event.QuestionEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON

instance ToJSON AddQuestionEventDTO where
  toJSON = toSumJSON

instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "type"
    case referenceType of
      "AddOptionsQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEventDTO' event)
      "AddListQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEventDTO' event)
      "AddValueQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEventDTO' event)
      "AddIntegrationQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddOptionsQuestionEventDTO where
  toJSON = simpleToJSON'' "_addOptionsQuestionEventDTO" [("type", "AddOptionsQuestionEvent")]

-- --------------------------------------------
instance FromJSON AddListQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddListQuestionEventDTO where
  toJSON = simpleToJSON'' "_addListQuestionEventDTO" [("type", "AddListQuestionEvent")]

-- --------------------------------------------
instance FromJSON AddValueQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddValueQuestionEventDTO where
  toJSON = simpleToJSON'' "_addValueQuestionEventDTO" [("type", "AddValueQuestionEvent")]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddIntegrationQuestionEventDTO where
  toJSON = simpleToJSON'' "_addIntegrationQuestionEventDTO" [("type", "AddIntegrationQuestionEvent")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEventDTO where
  toJSON = toSumJSON

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    questionType <- o .: "type"
    case questionType of
      "EditOptionsQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEventDTO' event)
      "EditListQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEventDTO' event)
      "EditValueQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEventDTO' event)
      "EditIntegrationQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditOptionsQuestionEventDTO where
  toJSON = simpleToJSON'' "_editOptionsQuestionEventDTO" [("type", "EditOptionsQuestionEvent")]

-- --------------------------------------------
instance FromJSON EditListQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditListQuestionEventDTO where
  toJSON = simpleToJSON'' "_editListQuestionEventDTO" [("type", "EditListQuestionEvent")]

-- --------------------------------------------
instance FromJSON EditValueQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditValueQuestionEventDTO where
  toJSON = simpleToJSON'' "_editValueQuestionEventDTO" [("type", "EditValueQuestionEvent")]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditIntegrationQuestionEventDTO where
  toJSON = simpleToJSON'' "_editIntegrationQuestionEventDTO" [("type", "EditIntegrationQuestionEvent")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DeleteQuestionEventDTO where
  toJSON = simpleToJSON' "_deleteQuestionEventDTO" "type"
