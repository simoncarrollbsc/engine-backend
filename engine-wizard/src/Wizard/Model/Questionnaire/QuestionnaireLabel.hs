module Wizard.Model.Questionnaire.QuestionnaireLabel where

import qualified Data.UUID as U
import GHC.Generics

data Label =
  Label
    { _path :: String
    , _value :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
