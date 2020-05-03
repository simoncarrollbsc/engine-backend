module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO =
  QuestionnaireCreateDTO
    { _name :: String
    , _packageId :: String
    , _accessibility :: QuestionnaireAccessibility
    , _tagUuids :: [U.UUID]
    , _templateUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
