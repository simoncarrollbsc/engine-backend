module Shared.Model.KnowledgeModel.KnowledgeModel where

import Data.Map
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

type KMParentMap = M.Map U.UUID U.UUID

data KnowledgeModel =
  KnowledgeModel
    { _uuid :: U.UUID
    , _name :: String
    , _chapterUuids :: [U.UUID]
    , _tagUuids :: [U.UUID]
    , _integrationUuids :: [U.UUID]
    , _entities :: KnowledgeModelEntities
    }
  deriving (Show, Eq, Generic)

data KnowledgeModelEntities =
  KnowledgeModelEntities
    { _chapters :: Map U.UUID Chapter
    , _questions :: Map U.UUID Question
    , _answers :: Map U.UUID Answer
    , _experts :: Map U.UUID Expert
    , _references :: Map U.UUID Reference
    , _integrations :: Map U.UUID Integration
    , _tags :: Map U.UUID Tag
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Chapter =
  Chapter
    { _uuid :: U.UUID
    , _title :: String
    , _text :: Maybe String
    , _questionUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic)

data Question
  = OptionsQuestion
      { _uuid :: U.UUID
      , _title :: String
      , _text :: Maybe String
      , _requiredLevel :: Maybe Int
      , _tagUuids :: [U.UUID]
      , _expertUuids :: [U.UUID]
      , _referenceUuids :: [U.UUID]
      , _answerUuids :: [U.UUID]
      }
  | ListQuestion
      { _uuid :: U.UUID
      , _title :: String
      , _text :: Maybe String
      , _requiredLevel :: Maybe Int
      , _tagUuids :: [U.UUID]
      , _expertUuids :: [U.UUID]
      , _referenceUuids :: [U.UUID]
      , _itemTemplateQuestionUuids :: [U.UUID]
      }
  | ValueQuestion
      { _uuid :: U.UUID
      , _title :: String
      , _text :: Maybe String
      , _requiredLevel :: Maybe Int
      , _tagUuids :: [U.UUID]
      , _expertUuids :: [U.UUID]
      , _referenceUuids :: [U.UUID]
      , _valueType :: QuestionValueType
      }
  | IntegrationQuestion
      { _uuid :: U.UUID
      , _title :: String
      , _text :: Maybe String
      , _requiredLevel :: Maybe Int
      , _tagUuids :: [U.UUID]
      , _expertUuids :: [U.UUID]
      , _referenceUuids :: [U.UUID]
      , _integrationUuid :: U.UUID
      , _integrationProps :: Map String String
      }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Answer =
  Answer
    { _uuid :: U.UUID
    , _label :: String
    , _advice :: Maybe String
    , _followUpUuids :: [U.UUID]
    , _metricMeasures :: [MetricMeasure]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Expert =
  Expert
    { _uuid :: U.UUID
    , _name :: String
    , _email :: String
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Reference
  = ResourcePageReference
      { _uuid :: U.UUID
      , _shortUuid :: String
      }
  | URLReference
      { _uuid :: U.UUID
      , _url :: String
      , _label :: String
      }
  | CrossReference
      { _uuid :: U.UUID
      , _targetUuid :: U.UUID
      , _description :: String
      }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Metric =
  Metric
    { _uuid :: U.UUID
    , _title :: String
    , _abbreviation :: Maybe String
    , _description :: Maybe String
    , _references :: [Reference]
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MetricMeasure =
  MetricMeasure
    { _metricUuid :: U.UUID
    , _measure :: Double
    , _weight :: Double
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Tag =
  Tag
    { _uuid :: U.UUID
    , _name :: String
    , _description :: Maybe String
    , _color :: String
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Integration =
  Integration
    { _uuid :: U.UUID
    , _iId :: String
    , _name :: String
    , _props :: [String]
    , _logo :: String
    , _requestMethod :: String
    , _requestUrl :: String
    , _requestHeaders :: Map String String
    , _requestBody :: String
    , _responseListField :: String
    , _responseIdField :: String
    , _responseNameField :: String
    , _itemUrl :: String
    }
  deriving (Show, Eq, Generic)