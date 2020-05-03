module Wizard.Service.Migration.Questionnaire.MigratorMapper where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Model.Migration.Questionnaire.MigratorState

toDTO :: QuestionnaireDetailDTO -> QuestionnaireDetailDTO -> [U.UUID] -> MigratorStateDTO
toDTO oldQtn newQtn qtnUuids =
  MigratorStateDTO {_oldQuestionnaire = oldQtn, _newQuestionnaire = newQtn, _resolvedQuestionUuids = qtnUuids}

fromCreateDTO :: U.UUID -> U.UUID -> MigratorState
fromCreateDTO oldQtnUuid newQtnUuid =
  MigratorState {_oldQuestionnaireUuid = oldQtnUuid, _newQuestionnaireUuid = newQtnUuid, _resolvedQuestionUuids = []}

fromChangeDTO :: MigratorStateChangeDTO -> MigratorStateDTO -> MigratorState
fromChangeDTO changeDto ms =
  MigratorState
    { _oldQuestionnaireUuid = ms ^. oldQuestionnaire . uuid
    , _newQuestionnaireUuid = ms ^. newQuestionnaire . uuid
    , _resolvedQuestionUuids = changeDto ^. resolvedQuestionUuids
    }
