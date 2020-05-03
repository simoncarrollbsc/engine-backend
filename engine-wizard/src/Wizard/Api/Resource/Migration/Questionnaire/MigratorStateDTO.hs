module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO

data MigratorStateDTO =
  MigratorStateDTO
    { _oldQuestionnaire :: QuestionnaireDetailDTO
    , _newQuestionnaire :: QuestionnaireDetailDTO
    , _resolvedQuestionUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
