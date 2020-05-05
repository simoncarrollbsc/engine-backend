module Shared.Database.Migration.Development.KnowledgeModel.Data.Tags where

import Control.Lens ((^.))
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

tagDataScience :: Tag
tagDataScience =
  Tag
    { _uuid = fromJust $ U.fromString "b2f3c232-018b-4d70-8e90-b5c81e8006f1"
    , _name = "Data Science"
    , _description = Just $ "Questions related to data science"
    , _color = "#4A90E2"
    }

tagDataScienceEdited :: Tag
tagDataScienceEdited =
  Tag
    { _uuid = tagDataScience ^. uuid
    , _name = "EDITED: " ++ (tagDataScience ^. name)
    , _description = Just $ "EDITED: " ++ (fromJust $ tagDataScience ^. description)
    , _color = "EDITED: " ++ (tagDataScience ^. color)
    }

tagBioInformatic :: Tag
tagBioInformatic =
  Tag
    { _uuid = fromJust $ U.fromString "e58abfb7-479d-4e81-95e0-83654e83da1a"
    , _name = "BioInformatic"
    , _description = Just $ "Questions related to bio informatic engineering"
    , _color = "#F5A623"
    }
