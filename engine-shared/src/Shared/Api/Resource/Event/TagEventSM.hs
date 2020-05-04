module Shared.Api.Resource.Event.TagEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.TagEventDTO
import Shared.Api.Resource.Event.TagEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddTagEventDTO where
  declareNamedSchema = simpleToSchema'' "_addTagEventDTO" "type" (toDTO a_km1_tds)

instance ToSchema EditTagEventDTO where
  declareNamedSchema = simpleToSchema'' "_editTagEventDTO" "type" (toDTO e_km1_tds)

instance ToSchema DeleteTagEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteTagEventDTO" "type" (toDTO d_km1_tds)
