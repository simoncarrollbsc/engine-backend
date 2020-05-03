module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO =
  QuestionnaireChangeDTO
    { _name :: String
    , _accessibility :: QuestionnaireAccessibility
    , _level :: Int
    , _replies :: [ReplyDTO]
    , _labels :: [LabelDTO]
    , _templateUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
