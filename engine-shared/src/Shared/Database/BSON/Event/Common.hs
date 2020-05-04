module Shared.Database.BSON.Event.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Database.BSON.Common ()
import Shared.Database.BSON.KnowledgeModel.KnowledgeModel ()


instance ToBSON Event where
  toBSON = toBSON'

instance FromBSON Event where
  fromBSON = fromBSON'

instance BSON.Val a => ToBSON (EventField a) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance BSON.Val a => FromBSON (EventField a) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

