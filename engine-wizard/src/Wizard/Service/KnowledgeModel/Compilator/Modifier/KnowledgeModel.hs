module Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Map as M

import LensesConfig
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity e =
    KnowledgeModel
      { _uuid = e ^. entityUuid
      , _name = e ^. name
      , _uuids = []
      , _tagUuids = []
      , _integrationUuids = []
      , _entities =
          KnowledgeModelEntities
            { _chapters = M.empty
            , _questions = M.empty
            , _answers = M.empty
            , _experts = M.empty
            , _references = M.empty
            , _integrations = M.empty
            , _tags = M.empty
            }
      }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity e = applyIntegrationUuids . applyTagUuids . applyChapterUuids . applyName
    where
      applyName km = applyValue (e ^. name) km name
      applyChapterUuids km = applyValue (e ^. chapterUuids) km chapterUuids
      applyTagUuids km = applyValue (e ^. tagUuids) km tagUuids
      applyIntegrationUuids km = applyValue (e ^. integrationUuids) km integrationUuids
