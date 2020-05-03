module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Model.Document.Document
import Wizard.Model.Template.Template

data DocumentDTO =
  DocumentDTO
    { _uuid :: U.UUID
    , _name :: String
    , _state :: DocumentState
    , _questionnaire :: Maybe QuestionnaireDTO
    , _template :: Template
    , _formatUuid :: U.UUID
    , _ownerUuid :: U.UUID
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
