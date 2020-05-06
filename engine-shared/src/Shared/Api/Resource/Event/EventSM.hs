module Shared.Api.Resource.Event.EventSM where

import Data.Swagger

import Shared.Model.Event.Event
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.Event.EventJM ()

instance ToSchema Event where
--  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
