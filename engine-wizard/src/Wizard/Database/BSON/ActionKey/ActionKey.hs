module Wizard.Database.BSON.ActionKey.ActionKey where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKey

instance ToBSON ActionKey where
  toBSON = toBSON'

instance FromBSON ActionKey where
  fromBSON = fromBSON'
