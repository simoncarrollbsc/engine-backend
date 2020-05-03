module Wizard.Database.BSON.Level.Level where

import Data.Bson.Generic

import Wizard.Model.Level.Level

instance ToBSON Level where
  toBSON = toBSON'

instance FromBSON Level where
  fromBSON = fromBSON'
