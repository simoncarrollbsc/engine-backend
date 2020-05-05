module Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddTagEvent Tag where
  createEntity e =
    Tag {_uuid = e ^. entityUuid, _name = e ^. name, _description = e ^. description, _color = e ^. color}

instance EditEntity EditTagEvent Tag where
  editEntity e = applyColor . applyDescription . applyName
    where
      applyName tag = applyValue (e ^. name) tag name
      applyDescription tag = applyValue (e ^. description) tag description
      applyColor tag = applyValue (e ^. color) tag color
