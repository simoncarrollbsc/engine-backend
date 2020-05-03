module Wizard.Api.Resource.Questionnaire.QuestionnaireDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import qualified Wizard.Api.Resource.Package.PackageSimpleDTO as PackageSimpleDTO
import qualified Wizard.Api.Resource.User.UserDTO as UserDTO
import qualified Wizard.Model.Questionnaire.Questionnaire as Questionnaire
import qualified Wizard.Model.Questionnaire.QuestionnaireState as QuestionnaireState

data QuestionnaireDTO =
  QuestionnaireDTO
    { _uuid :: U.UUID
    , _name :: String
    , _level :: Int
    , _accessibility :: Questionnaire.QuestionnaireAccessibility
    , _state :: QuestionnaireState.QuestionnaireState
    , _package :: PackageSimpleDTO.PackageSimpleDTO
    , _owner :: Maybe UserDTO.UserDTO
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireDTO where
  a == b =
    _uuid a == _uuid b &&
    _name a == _name b &&
    _level a == _level b &&
    _accessibility a == _accessibility b && _state a == _state b && _package a == _package b && _owner a == _owner b
