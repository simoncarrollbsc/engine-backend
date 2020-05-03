module Registry.Database.BSON.ActionKey.ActionKey where

import Data.Bson.Generic

import Registry.Database.BSON.ActionKey.ActionKeyType ()
import Registry.Model.ActionKey.ActionKey
import Shared.Database.BSON.Common ()

instance ToBSON ActionKey where
  toBSON = toBSON'

instance FromBSON ActionKey where
  fromBSON = fromBSON'
