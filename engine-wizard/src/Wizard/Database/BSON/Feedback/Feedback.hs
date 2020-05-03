module Wizard.Database.BSON.Feedback.Feedback where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Feedback.Feedback

instance ToBSON Feedback where
  toBSON = toBSON'

instance FromBSON Feedback where
  fromBSON = fromBSON'
