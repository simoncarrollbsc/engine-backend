module Wizard.Database.BSON.Branch.Branch where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Branch.Branch

instance ToBSON Branch where
  toBSON = toBSON'

instance FromBSON Branch where
  fromBSON = fromBSON'
