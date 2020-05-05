module Shared.Model.Event.Event where

import GHC.Generics

import qualified Data.UUID as U
import Data.Map.Strict
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Event.EventField

data Event
  = AddKnowledgeModelEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      }
  | EditKnowledgeModelEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      , _chapterUuids :: EventField [U.UUID]
      , _tagUuids :: EventField [U.UUID]
      , _integrationUuids :: EventField [U.UUID]
      }
  | AddChapterEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      }
  | EditChapterEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _eitle :: EventField String
      , _eext :: EventField (Maybe String)
      , _questionUuids :: EventField [U.UUID]
      }
  | DeleteChapterEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddOptionsQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      }
  | AddListQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      }
  | AddValueQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _valueType :: EventField QuestionValueType
      }
  | AddIntegrationQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _integrationUuid :: EventField U.UUID
      , _integrationProps :: EventField (Map String String)
      }
  | EditOptionsQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _expertUuids :: EventField [U.UUID]
      , _referenceUuids :: EventField [U.UUID]
      , _answerUuids :: EventField [U.UUID]
      }
  | EditListQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _expertUuids :: EventField [U.UUID]
      , _referenceUuids :: EventField [U.UUID]
      , _itemTemplateQuestionUuids :: EventField [U.UUID]
      }
  | EditValueQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _expertUuids :: EventField [U.UUID]
      , _referenceUuids :: EventField [U.UUID]
      , _valueType :: EventField QuestionValueType
      }
  | EditIntegrationQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _title :: EventField String
      , _text :: EventField (Maybe String)
      , _requiredLevel :: EventField (Maybe Int)
      , _tagUuids :: EventField [U.UUID]
      , _expertUuids :: EventField [U.UUID]
      , _referenceUuids :: EventField [U.UUID]
      , _integrationUuid :: EventField U.UUID
      , _integrationProps :: EventField (Map String String)
      }
  | DeleteQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddAnswerEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _label :: EventField String
      , _advice :: EventField (Maybe String)
      , _metricMeasures :: EventField [MetricMeasure]
      }
  | EditAnswerEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _label :: EventField String
      , _advice :: EventField (Maybe String)
      , _followUpUuids :: EventField [U.UUID]
      , _metricMeasures :: EventField [MetricMeasure]
      }
  | DeleteAnswerEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddExpertEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      , _email :: EventField String
      }
  | EditExpertEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      , _email :: EventField String
      }
  | DeleteExpertEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddResourcePageReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _shortUuid :: EventField String
      }
  | AddURLReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _url :: EventField String
      , _label :: EventField String
      }
  | AddCrossReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      , _label :: EventField String
      }
  | EditResourcePageReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _shortUuid :: EventField String
      }
  | EditURLReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _url :: EventField String
      , _label :: EventField String
      }
  | EditCrossReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      , _label :: EventField String
      }
  | DeleteReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddTagEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      , _description :: EventField (Maybe String)
      , _color :: EventField String
      }
  | EditTagEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _name :: EventField String
      , _description :: EventField (Maybe String)
      , _color :: EventField String
      }
  | DeleteTagEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | AddIntegrationEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _iId :: EventField String
      , _name :: EventField String
      , _props :: EventField [String]
      , _logo :: EventField String
      , _requestMethod :: EventField String
      , _requestUrl :: EventField String
      , _requestHeaders :: EventField (Map String String)
      , _requestBody :: EventField String
      , _responseListField :: EventField String
      , _responseIdField :: EventField String
      , _responseNameField :: EventField String
      , _itemUrl :: EventField String
      }
  | EditIntegrationEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _iId :: EventField String
      , _name :: EventField String
      , _props :: EventField [String]
      , _logo :: EventField String
      , _requestMethod :: EventField String
      , _requestUrl :: EventField String
      , _requestHeaders :: EventField (Map String String)
      , _requestBody :: EventField String
      , _responseListField :: EventField String
      , _responseIdField :: EventField String
      , _responseNameField :: EventField String
      , _itemUrl :: EventField String
      }
  | DeleteIntegrationEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      }
  | MoveQuestionEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      }
  | MoveAnswerEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      }
  | MoveExpertEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      }
  | MoveReferenceEvent
      { _uuid :: U.UUID
      , _parentUuid :: U.UUID
      , _entityUuid :: U.UUID
      , _targetUuid :: EventField U.UUID
      }
  deriving (Show, Eq, Generic)
