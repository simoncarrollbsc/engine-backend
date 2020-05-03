module Wizard.Database.BSON.Questionnaire.QuestionnaireLabel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Questionnaire.QuestionnaireLabel

instance FromBSON Label where
  fromBSON doc = do
    _path <- BSON.lookup "path" doc
    _value <- BSON.lookup "value" doc
    return Label {..}
  fromBSON' doc = do
    _path <- BSON.lookup "path" doc
    _value <- BSON.lookup "value" doc
    return Label {..}

instance ToBSON Label where
  toBSON Label {..} = ["path" BSON.=: _path, "value" BSON.=: _value]
  toBSON' Label {..} = ["path" BSON.=: _path, "value" BSON.=: _value]
