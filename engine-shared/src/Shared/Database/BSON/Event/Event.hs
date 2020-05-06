module Shared.Database.BSON.Event.Event where

import Data.Bson.Generic

import Shared.Model.Event.Event
import Shared.Database.BSON.Event.EventField ()
import Shared.Database.BSON.Common ()
import Shared.Database.BSON.KnowledgeModel.KnowledgeModel ()

instance ToBSON Event

instance FromBSON Event

