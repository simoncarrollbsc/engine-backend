module Wizard.Database.BSON.Migration.Questionnaire.MigratorState where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Questionnaire.Questionnaire ()
import Wizard.Model.Migration.Questionnaire.MigratorState

instance ToBSON MigratorState where
  toBSON = toBSON'

instance FromBSON MigratorState where
  fromBSON = fromBSON'
