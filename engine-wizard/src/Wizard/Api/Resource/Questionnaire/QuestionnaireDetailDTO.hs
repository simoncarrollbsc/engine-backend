module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import qualified Shared.Model.KnowledgeModel.KnowledgeModel as KnowledgeModel
import qualified Wizard.Api.Resource.Package.PackageSimpleDTO as PackageSimpleDTO
import qualified Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO as QuestionnaireLabelDTO
import qualified Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO as QuestionnaireReplyDTO
import qualified Wizard.Model.Questionnaire.Questionnaire as Questionnaire
import qualified Wizard.Model.Questionnaire.QuestionnaireState as QuestionnaireState

data QuestionnaireDetailDTO =
  QuestionnaireDetailDTO
    { _uuid :: U.UUID
    , _name :: String
    , _level :: Int
    , _accessibility :: Questionnaire.QuestionnaireAccessibility
    , _state :: QuestionnaireState.QuestionnaireState
    , _package :: PackageSimpleDTO.PackageSimpleDTO
    , _selectedTagUuids :: [U.UUID]
    , _templateUuid :: Maybe U.UUID
    , _formatUuid :: Maybe U.UUID
    , _e :: KnowledgeModel.KnowledgeModel
    , _replies :: [QuestionnaireReplyDTO.ReplyDTO]
    , _labels :: [QuestionnaireLabelDTO.LabelDTO]
    , _ownerUuid :: Maybe U.UUID
    , _creatorUuid :: Maybe U.UUID
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireDetailDTO where
  a == b =
    _uuid a == _uuid b &&
    _name a == _name b &&
    _level a == _level b &&
    _accessibility a == _accessibility b &&
    _state a == _state b &&
    _package a == _package b &&
    _selectedTagUuids a == _selectedTagUuids b &&
    _templateUuid a == _templateUuid b &&
    _formatUuid a == _formatUuid b &&
    _e a == _e b &&
    _replies a == _replies b && _ownerUuid a == _ownerUuid b && _creatorUuid a == _creatorUuid b
