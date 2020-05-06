module Shared.Model.Event.Event where

import GHC.Generics

import Data.Map.Strict
import qualified Data.UUID as U
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel

data Event
  = AddKnowledgeModelEvent
      { _addKnowledgeModelEventUuid :: U.UUID
      , _addKnowledgeModelEventParentUuid :: U.UUID
      , _addKnowledgeModelEventEntityUuid :: U.UUID
      , _addKnowledgeModelEventName :: String
      }
  | EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid :: U.UUID
      , _editKnowledgeModelEventParentUuid :: U.UUID
      , _editKnowledgeModelEventEntityUuid :: U.UUID
      , _editKnowledgeModelEventName :: EventField String
      , _editKnowledgeModelEventChapterUuids :: EventField [U.UUID]
      , _editKnowledgeModelEventTagUuids :: EventField [U.UUID]
      , _editKnowledgeModelEventIntegrationUuids :: EventField [U.UUID]
      }
  | AddChapterEvent
      { _addChapterEventUuid :: U.UUID
      , _addChapterEventParentUuid :: U.UUID
      , _addChapterEventEntityUuid :: U.UUID
      , _addChapterEventTitle :: String
      , _addChapterEventText :: Maybe String
      }
  | EditChapterEvent
      { _editChapterEventUuid :: U.UUID
      , _editChapterEventParentUuid :: U.UUID
      , _editChapterEventEntityUuid :: U.UUID
      , _editChapterEventTitle :: EventField String
      , _editChapterEventText :: EventField (Maybe String)
      , _editChapterEventQuestionUuids :: EventField [U.UUID]
      }
  | DeleteChapterEvent
      { _deleteChapterEventUuid :: U.UUID
      , _deleteChapterEventParentUuid :: U.UUID
      , _deleteChapterEventEntityUuid :: U.UUID
      }
  | AddOptionsQuestionEvent
      { _addOptionsQuestionEventUuid :: U.UUID
      , _addOptionsQuestionEventParentUuid :: U.UUID
      , _addOptionsQuestionEventEntityUuid :: U.UUID
      , _addOptionsQuestionEventTitle :: String
      , _addOptionsQuestionEventText :: Maybe String
      , _addOptionsQuestionEventRequiredLevel :: Maybe Int
      , _addOptionsQuestionEventTagUuids :: [U.UUID]
      }
  | AddListQuestionEvent
      { _addListQuestionEventUuid :: U.UUID
      , _addListQuestionEventParentUuid :: U.UUID
      , _addListQuestionEventEntityUuid :: U.UUID
      , _addListQuestionEventTitle :: String
      , _addListQuestionEventText :: Maybe String
      , _addListQuestionEventRequiredLevel :: Maybe Int
      , _addListQuestionEventTagUuids :: [U.UUID]
      }
  | AddValueQuestionEvent
      { _addValueQuestionEventUuid :: U.UUID
      , _addValueQuestionEventParentUuid :: U.UUID
      , _addValueQuestionEventEntityUuid :: U.UUID
      , _addValueQuestionEventTitle :: String
      , _addValueQuestionEventText :: Maybe String
      , _addValueQuestionEventRequiredLevel :: Maybe Int
      , _addValueQuestionEventTagUuids :: [U.UUID]
      , _addValueQuestionEventValueType :: QuestionValueType
      }
  | AddIntegrationQuestionEvent
      { _addIntegrationQuestionEventUuid :: U.UUID
      , _addIntegrationQuestionEventParentUuid :: U.UUID
      , _addIntegrationQuestionEventEntityUuid :: U.UUID
      , _addIntegrationQuestionEventTitle :: String
      , _addIntegrationQuestionEventText :: Maybe String
      , _addIntegrationQuestionEventRequiredLevel :: Maybe Int
      , _addIntegrationQuestionEventTagUuids :: [U.UUID]
      , _addIntegrationQuestionEventIntegrationUuid :: U.UUID
      , _addIntegrationQuestionEventProps :: Map String String
      }
  | EditOptionsQuestionEvent
      { _editOptionsQuestionEventUuid :: U.UUID
      , _editOptionsQuestionEventParentUuid :: U.UUID
      , _editOptionsQuestionEventEntityUuid :: U.UUID
      , _editOptionsQuestionEventTitle :: EventField String
      , _editOptionsQuestionEventText :: EventField (Maybe String)
      , _editOptionsQuestionEventRequiredLevel :: EventField (Maybe Int)
      , _editOptionsQuestionEventTagUuids :: EventField [U.UUID]
      , _editOptionsQuestionEventExpertUuids :: EventField [U.UUID]
      , _editOptionsQuestionEventReferenceUuids :: EventField [U.UUID]
      , _editOptionsQuestionEventAnswerUuids :: EventField [U.UUID]
      }
  | EditListQuestionEvent
      { _editListQuestionEventUuid :: U.UUID
      , _editListQuestionEventParentUuid :: U.UUID
      , _editListQuestionEventEntityUuid :: U.UUID
      , _editListQuestionEventTitle :: EventField String
      , _editListQuestionEventText :: EventField (Maybe String)
      , _editListQuestionEventRequiredLevel :: EventField (Maybe Int)
      , _editListQuestionEventTagUuids :: EventField [U.UUID]
      , _editListQuestionEventExpertUuids :: EventField [U.UUID]
      , _editListQuestionEventReferenceUuids :: EventField [U.UUID]
      , _editListQuestionEventItemTemplateQuestionUuids :: EventField [U.UUID]
      }
  | EditValueQuestionEvent
      { _editValueQuestionEventUuid :: U.UUID
      , _editValueQuestionEventParentUuid :: U.UUID
      , _editValueQuestionEventEntityUuid :: U.UUID
      , _editValueQuestionEventTitle :: EventField String
      , _editValueQuestionEventText :: EventField (Maybe String)
      , _editValueQuestionEventRequiredLevel :: EventField (Maybe Int)
      , _editValueQuestionEventTagUuids :: EventField [U.UUID]
      , _editValueQuestionEventExpertUuids :: EventField [U.UUID]
      , _editValueQuestionEventReferenceUuids :: EventField [U.UUID]
      , _editValueQuestionEventValueType :: EventField QuestionValueType
      }
  | EditIntegrationQuestionEvent
      { _editIntegrationQuestionEventUuid :: U.UUID
      , _editIntegrationQuestionEventParentUuid :: U.UUID
      , _editIntegrationQuestionEventEntityUuid :: U.UUID
      , _editIntegrationQuestionEventTitle :: EventField String
      , _editIntegrationQuestionEventText :: EventField (Maybe String)
      , _editIntegrationQuestionEventRequiredLevel :: EventField (Maybe Int)
      , _editIntegrationQuestionEventTagUuids :: EventField [U.UUID]
      , _editIntegrationQuestionEventExpertUuids :: EventField [U.UUID]
      , _editIntegrationQuestionEventReferenceUuids :: EventField [U.UUID]
      , _editIntegrationQuestionEventIntegrationUuid :: EventField U.UUID
      , _editIntegrationQuestionEventProps :: EventField (Map String String)
      }
  | DeleteQuestionEvent
      { _deleteQuestionEventUuid :: U.UUID
      , _deleteQuestionEventParentUuid :: U.UUID
      , _deleteQuestionEventEntityUuid :: U.UUID
      }
  | AddAnswerEvent
      { _addAnswerEventUuid :: U.UUID
      , _addAnswerEventParentUuid :: U.UUID
      , _addAnswerEventEntityUuid :: U.UUID
      , _addAnswerEventLabel :: String
      , _addAnswerEventAdvice :: Maybe String
      , _addAnswerEventMetricMeasures :: [MetricMeasure]
      }
  | EditAnswerEvent
      { _editAnswerEventUuid :: U.UUID
      , _editAnswerEventParentUuid :: U.UUID
      , _editAnswerEventEntityUuid :: U.UUID
      , _editAnswerEventLabel :: EventField String
      , _editAnswerEventAdvice :: EventField (Maybe String)
      , _editAnswerEventFollowUpUuids :: EventField [U.UUID]
      , _editAnswerEventMetricMeasures :: EventField [MetricMeasure]
      }
  | DeleteAnswerEvent
      { _deleteAnswerEventUuid :: U.UUID
      , _deleteAnswerEventParentUuid :: U.UUID
      , _deleteAnswerEventEntityUuid :: U.UUID
      }
  | AddExpertEvent
      { _addExpertEventUuid :: U.UUID
      , _addExpertEventParentUuid :: U.UUID
      , _addExpertEventEntityUuid :: U.UUID
      , _addExpertEventName :: String
      , _addExpertEventEmail :: String
      }
  | EditExpertEvent
      { _editExpertEventUuid :: U.UUID
      , _editExpertEventParentUuid :: U.UUID
      , _editExpertEventEntityUuid :: U.UUID
      , _editExpertEventName :: EventField String
      , _editExpertEventEmail :: EventField String
      }
  | DeleteExpertEvent
      { _deleteExpertEventUuid :: U.UUID
      , _deleteExpertEventParentUuid :: U.UUID
      , _deleteExpertEventEntityUuid :: U.UUID
      }
  | AddResourcePageReferenceEvent
      { _addResourcePageReferenceEventUuid :: U.UUID
      , _addResourcePageReferenceEventParentUuid :: U.UUID
      , _addResourcePageReferenceEventEntityUuid :: U.UUID
      , _addResourcePageReferenceEventShortUuid :: String
      }
  | AddURLReferenceEvent
      { _addURLReferenceEventUuid :: U.UUID
      , _addURLReferenceEventParentUuid :: U.UUID
      , _addURLReferenceEventEntityUuid :: U.UUID
      , _addURLReferenceEventUrl :: String
      , _addURLReferenceEventLabel :: String
      }
  | AddCrossReferenceEvent
      { _addCrossReferenceEventUuid :: U.UUID
      , _addCrossReferenceEventParentUuid :: U.UUID
      , _addCrossReferenceEventEntityUuid :: U.UUID
      , _addCrossReferenceEventTargetUuid :: U.UUID
      , _addCrossReferenceEventDescription :: String
      }
  | EditResourcePageReferenceEvent
      { _editResourcePageReferenceEventUuid :: U.UUID
      , _editResourcePageReferenceEventParentUuid :: U.UUID
      , _editResourcePageReferenceEventEntityUuid :: U.UUID
      , _editResourcePageReferenceEventShortUuid :: EventField String
      }
  | EditURLReferenceEvent
      { _editURLReferenceEventUuid :: U.UUID
      , _editURLReferenceEventParentUuid :: U.UUID
      , _editURLReferenceEventEntityUuid :: U.UUID
      , _editURLReferenceEventUrl :: EventField String
      , _editURLReferenceEventLabel :: EventField String
      }
  | EditCrossReferenceEvent
      { _editCrossReferenceEventUuid :: U.UUID
      , _editCrossReferenceEventParentUuid :: U.UUID
      , _editCrossReferenceEventEntityUuid :: U.UUID
      , _editCrossReferenceEventTargetUuid :: EventField U.UUID
      , _editCrossReferenceEventDescription :: EventField String
      }
  | DeleteReferenceEvent
      { _deleteReferenceEventUuid :: U.UUID
      , _deleteReferenceEventParentUuid :: U.UUID
      , _deleteReferenceEventEntityUuid :: U.UUID
      }
  | AddTagEvent
      { _addTagEventUuid :: U.UUID
      , _addTagEventParentUuid :: U.UUID
      , _addTagEventEntityUuid :: U.UUID
      , _addTagEventName :: String
      , _addTagEventDescription :: Maybe String
      , _addTagEventColor :: String
      }
  | EditTagEvent
      { _editTagEventUuid :: U.UUID
      , _editTagEventParentUuid :: U.UUID
      , _editTagEventEntityUuid :: U.UUID
      , _editTagEventName :: EventField String
      , _editTagEventDescription :: EventField (Maybe String)
      , _editTagEventColor :: EventField String
      }
  | DeleteTagEvent
      { _deleteTagEventUuid :: U.UUID
      , _deleteTagEventParentUuid :: U.UUID
      , _deleteTagEventEntityUuid :: U.UUID
      }
  | AddIntegrationEvent
      { _addIntegrationEventUuid :: U.UUID
      , _addIntegrationEventParentUuid :: U.UUID
      , _addIntegrationEventEntityUuid :: U.UUID
      , _addIntegrationEventIId :: String
      , _addIntegrationEventName :: String
      , _addIntegrationEventProps :: [String]
      , _addIntegrationEventLogo :: String
      , _addIntegrationEventRequestMethod :: String
      , _addIntegrationEventRequestUrl :: String
      , _addIntegrationEventRequestHeaders :: Map String String
      , _addIntegrationEventRequestBody :: String
      , _addIntegrationEventResponseListField :: String
      , _addIntegrationEventResponseIdField :: String
      , _addIntegrationEventResponseNameField :: String
      , _addIntegrationEventItemUrl :: String
      }
  | EditIntegrationEvent
      { _editIntegrationEventUuid :: U.UUID
      , _editIntegrationEventParentUuid :: U.UUID
      , _editIntegrationEventEntityUuid :: U.UUID
      , _editIntegrationEventIId :: EventField String
      , _editIntegrationEventName :: EventField String
      , _editIntegrationEventProps :: EventField [String]
      , _editIntegrationEventLogo :: EventField String
      , _editIntegrationEventRequestMethod :: EventField String
      , _editIntegrationEventRequestUrl :: EventField String
      , _editIntegrationEventRequestHeaders :: EventField (Map String String)
      , _editIntegrationEventRequestBody :: EventField String
      , _editIntegrationEventResponseListField :: EventField String
      , _editIntegrationEventResponseIdField :: EventField String
      , _editIntegrationEventResponseNameField :: EventField String
      , _editIntegrationEventItemUrl :: EventField String
      }
  | DeleteIntegrationEvent
      { _deleteIntegrationEventUuid :: U.UUID
      , _deleteIntegrationEventParentUuid :: U.UUID
      , _deleteIntegrationEventEntityUuid :: U.UUID
      }
  | MoveQuestionEvent
      { _moveQuestionEventUuid :: U.UUID
      , _moveQuestionEventParentUuid :: U.UUID
      , _moveQuestionEventEntityUuid :: U.UUID
      , _moveQuestionEventTargetUuid :: U.UUID
      }
  | MoveAnswerEvent
      { _moveAnswerEventUuid :: U.UUID
      , _moveAnswerEventParentUuid :: U.UUID
      , _moveAnswerEventEntityUuid :: U.UUID
      , _moveAnswerEventTargetUuid :: U.UUID
      }
  | MoveExpertEvent
      { _moveExpertEventUuid :: U.UUID
      , _moveExpertEventParentUuid :: U.UUID
      , _moveExpertEventEntityUuid :: U.UUID
      , _moveExpertEventTargetUuid :: U.UUID
      }
  | MoveReferenceEvent
      { _moveReferenceEventUuid :: U.UUID
      , _moveReferenceEventParentUuid :: U.UUID
      , _moveReferenceEventEntityUuid :: U.UUID
      , _moveReferenceEventTargetUuid :: U.UUID
      }
  deriving (Show, Eq, Generic)