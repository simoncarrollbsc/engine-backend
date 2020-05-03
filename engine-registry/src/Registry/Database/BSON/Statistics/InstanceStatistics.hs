module Registry.Database.BSON.Statistics.InstanceStatistics where

import Data.Bson.Generic

import Registry.Model.Statistics.InstanceStatistics

instance ToBSON InstanceStatistics where
  toBSON = toBSON'

instance FromBSON InstanceStatistics where
  fromBSON = fromBSON'
