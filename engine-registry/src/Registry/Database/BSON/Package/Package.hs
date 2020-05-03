module Registry.Database.BSON.Package.Package where

import Data.Bson.Generic

import Shared.Model.Package.Package

instance ToBSON Package where
  toBSON = toBSON'

instance FromBSON Package where
  fromBSON = fromBSON'
