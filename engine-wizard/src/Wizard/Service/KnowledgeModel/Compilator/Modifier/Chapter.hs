module Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChapterEvent Chapter where
  createEntity e =
    Chapter
      {_uuid = e ^. entityUuid, _title = e ^. title, _text = e ^. text, _questionUuids = []}

instance EditEntity EditChapterEvent Chapter where
  editEntity e = applyQuestionUuids . applyText . applyTitle
    where
      applyTitle ch = applyValue (e ^. title) ch title
      applyText ch = applyValue (e ^. text) ch text
      applyQuestionUuids ch = applyValue (e ^. questionUuids) ch questionUuids
