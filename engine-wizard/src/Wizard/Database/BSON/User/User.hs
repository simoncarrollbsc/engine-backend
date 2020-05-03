module Wizard.Database.BSON.User.User where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.User.User

instance ToBSON User where
  toBSON = toBSON'

instance FromBSON User where
  fromBSON = fromBSON'

instance ToBSON UserSubmissionProps where
  toBSON = toBSON'

instance FromBSON UserSubmissionProps where
  fromBSON = fromBSON'
