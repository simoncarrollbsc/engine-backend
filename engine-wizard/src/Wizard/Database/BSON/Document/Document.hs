module Wizard.Database.BSON.Document.Document where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Document.DocumentType ()
import Wizard.Model.Document.Document

instance ToBSON DocumentMetadata where
  toBSON = toBSON'

instance FromBSON DocumentMetadata where
  fromBSON = fromBSON'

instance ToBSON Document where
  toBSON = toBSON'

instance FromBSON Document where
  fromBSON = fromBSON'
