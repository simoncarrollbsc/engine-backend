module Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

nlQtnMigrationState :: MigratorState
nlQtnMigrationState =
  MigratorState
    { _oldQuestionnaireUuid = nlQtnMigrationStateDto ^. oldQuestionnaire . uuid
    , _newQuestionnaireUuid = nlQtnMigrationStateDto ^. newQuestionnaire . uuid
    , _resolvedQuestionUuids = [question2 ^. uuid]
    }

nlQtnMigrationStateDto :: MigratorStateDTO
nlQtnMigrationStateDto =
  MigratorStateDTO
    { _oldQuestionnaire = toDetailWithPackageWithEventsDTO questionnaire4 netherlandsPackage km1Netherlands QSOutdated
    , _newQuestionnaire =
        toDetailWithPackageWithEventsDTO questionnaire4Upgraded netherlandsPackageV2 km1NetherlandsV2 QSMigrating
    , _resolvedQuestionUuids = [question2 ^. uuid]
    }

nlQtnMigrationStatePublicReadOnlyDto :: MigratorStateDTO
nlQtnMigrationStatePublicReadOnlyDto =
  MigratorStateDTO
    { _oldQuestionnaire =
        toDetailWithPackageWithEventsDTO questionnaire4PublicReadOnly netherlandsPackage km1Netherlands QSOutdated
    , _newQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4PublicReadOnlyUpgraded
          netherlandsPackageV2
          km1NetherlandsV2
          QSMigrating
    , _resolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    }

nlQtnMigrationStatePublicDto :: MigratorStateDTO
nlQtnMigrationStatePublicDto =
  MigratorStateDTO
    { _oldQuestionnaire =
        toDetailWithPackageWithEventsDTO questionnaire4Public netherlandsPackage km1Netherlands QSOutdated
    , _newQuestionnaire =
        toDetailWithPackageWithEventsDTO questionnaire4PublicUpgraded netherlandsPackageV2 km1NetherlandsV2 QSMigrating
    , _resolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
    { _oldQuestionnaire = nlQtnMigrationStateDto ^. oldQuestionnaire
    , _newQuestionnaire = nlQtnMigrationStateDto ^. newQuestionnaire
    , _resolvedQuestionUuids = [question2 ^. uuid, question3 ^. uuid]
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate =
  MigratorStateCreateDTO
    {_targetPackageId = netherlandsPackageV2 ^. pId, _targetTagUuids = questionnaire4Upgraded ^. selectedTagUuids}

migratorStateChange :: MigratorStateChangeDTO
migratorStateChange =
  MigratorStateChangeDTO {_resolvedQuestionUuids = nlQtnMigrationStateDtoEdited ^. resolvedQuestionUuids}
