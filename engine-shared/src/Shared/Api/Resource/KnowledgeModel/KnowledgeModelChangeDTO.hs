module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data KnowledgeModelChangeDTO =
  KnowledgeModelChangeDTO
    { _packageId :: Maybe String
    , _events :: [EventDTO]
    , _tagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
