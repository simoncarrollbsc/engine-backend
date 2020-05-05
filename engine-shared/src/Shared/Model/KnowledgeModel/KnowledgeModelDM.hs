module Shared.Model.KnowledgeModel.KnowledgeModelDM where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Shared.Model.KnowledgeModel.KnowledgeModel

defaultKnowledgeModel :: KnowledgeModel
defaultKnowledgeModel =
  KnowledgeModel
    { _uuid = U.nil
    , _name = ""
    , _chapterUuids = []
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
