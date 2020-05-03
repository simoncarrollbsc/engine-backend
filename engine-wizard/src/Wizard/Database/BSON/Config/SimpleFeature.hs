module Wizard.Database.BSON.Config.SimpleFeature where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Config.SimpleFeature

instance ToBSON SimpleFeature where
  toBSON SimpleFeature {..} = ["enabled" BSON.=: _enabled]

instance FromBSON SimpleFeature where
  fromBSON doc = do
    _enabled <- BSON.lookup "enabled" doc
    return SimpleFeature {..}
