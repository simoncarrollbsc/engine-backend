module Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag where

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity Event Tag where
  createEntity e@AddTagEvent {..} =
    Tag
      { _tagUuid = _addTagEventEntityUuid
      , _tagName = _addTagEventName
      , _tagDescription = _addTagEventDescription
      , _tagColor = _addTagEventColor
      }

instance EditEntity Event Tag where
  editEntity e@EditTagEvent {..} = applyColor . applyDescription . applyName
    where
      applyName tag = applyValue _editTagEventName tag name
      applyDescription tag = applyValue _editTagEventDescription tag description
      applyColor tag = applyValue _editTagEventColor tag color