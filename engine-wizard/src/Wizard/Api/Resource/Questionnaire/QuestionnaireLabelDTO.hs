module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO where

import qualified Data.UUID as U
import GHC.Generics

data LabelDTO =
  LabelDTO
    { _path :: String
    , _value :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
