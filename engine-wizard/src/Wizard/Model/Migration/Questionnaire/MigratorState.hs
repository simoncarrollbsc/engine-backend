module Wizard.Model.Migration.Questionnaire.MigratorState where

import qualified Data.UUID as U
import GHC.Generics

data MigratorState =
  MigratorState
    { _oldQuestionnaireUuid :: U.UUID
    , _newQuestionnaireUuid :: U.UUID
    , _resolvedQuestionUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
