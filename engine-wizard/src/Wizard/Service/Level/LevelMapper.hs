module Wizard.Service.Level.LevelMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Model.Level.Level

toLevelDTO :: Level -> LevelDTO
toLevelDTO l =
  LevelDTO
    { _level = l ^. level
    , _title = l ^. title
    , _description = l ^. description
    , _createdAt = l ^. createdAt
    , _updatedAt = l ^. updatedAt
    }
