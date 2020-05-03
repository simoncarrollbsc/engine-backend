module Wizard.Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireAccessibility
  = PublicQuestionnaire
  | PrivateQuestionnaire
  | PublicReadOnlyQuestionnaire
  deriving (Show, Eq, Generic)

data Questionnaire =
  Questionnaire
    { _uuid :: U.UUID
    , _name :: String
    , _level :: Int
    , _accessibility :: QuestionnaireAccessibility
    , _packageId :: String
    , _selectedTagUuids :: [U.UUID]
    , _templateUuid :: Maybe U.UUID
    , _formatUuid :: Maybe U.UUID
    , _ownerUuid :: Maybe U.UUID
    , _creatorUuid :: Maybe U.UUID
    , _replies :: [Reply]
    , _labels :: [Label]
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _uuid a == _uuid b &&
    _name a == _name b &&
    _level a == _level b &&
    _accessibility a == _accessibility b &&
    _packageId a == _packageId b &&
    _selectedTagUuids a == _selectedTagUuids b &&
    _templateUuid a == _templateUuid b &&
    _formatUuid a == _formatUuid b &&
    _ownerUuid a == _ownerUuid b &&
    _creatorUuid a == _creatorUuid b && _replies a == _replies b && _labels a == _labels b
