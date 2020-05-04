module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data KnowledgeModelChangeDTO =
  KnowledgeModelChangeDTO
    { _packageId :: Maybe String
    , _events :: [Event]
    , _tagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
