module Wizard.Database.BSON.Branch.BranchWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.Common
import Wizard.Model.Branch.Branch

instance ToBSON BranchWithEvents where
  toBSON BranchWithEvents {..} =
    [ "uuid" BSON.=: _uuid
    , "name" BSON.=: _name
    , "kmId" BSON.=: _kmId
    , "metamodelVersion" BSON.=: _metamodelVersion
    , "previousPackageId" BSON.=: _previousPackageId
    , "events" BSON.=: convertEventToBSON <$> _events
    , "ownerUuid" BSON.=: _ownerUuid
    , "createdAt" BSON.=: _createdAt
    , "updatedAt" BSON.=: _updatedAt
    ]
  toBSON' BranchWithEvents {..} =
    [ "uuid" BSON.=: _uuid
    , "name" BSON.=: _name
    , "kmId" BSON.=: _kmId
    , "metamodelVersion" BSON.=: _metamodelVersion
    , "previousPackageId" BSON.=: _previousPackageId
    , "events" BSON.=: convertEventToBSON <$> _events
    , "ownerUuid" BSON.=: _ownerUuid
    , "createdAt" BSON.=: _createdAt
    , "updatedAt" BSON.=: _updatedAt
    ]

instance FromBSON BranchWithEvents where
  fromBSON doc = do
    _uuid <- BSON.lookup "uuid" doc
    _name <- BSON.lookup "name" doc
    _kmId <- BSON.lookup "kmId" doc
    _metamodelVersion <- BSON.lookup "metamodelVersion" doc
    _previousPackageId <- BSON.lookup "previousPackageId" doc
    eventsSerialized <- BSON.lookup "events" doc
    let _events = fmap (fromJust . chooseEventDeserializator) eventsSerialized
    _ownerUuid <- BSON.lookup "ownerUuid" doc
    _createdAt <- BSON.lookup "createdAt" doc
    _updatedAt <- BSON.lookup "updatedAt" doc
    return BranchWithEvents {..}
  fromBSON' doc = do
    _uuid <- BSON.lookup "uuid" doc
    _name <- BSON.lookup "name" doc
    _kmId <- BSON.lookup "kmId" doc
    _metamodelVersion <- BSON.lookup "metamodelVersion" doc
    _previousPackageId <- BSON.lookup "previousPackageId" doc
    eventsSerialized <- BSON.lookup "events" doc
    let _events = fmap (fromJust . chooseEventDeserializator) eventsSerialized
    _ownerUuid <- BSON.lookup "ownerUuid" doc
    _createdAt <- BSON.lookup "createdAt" doc
    _updatedAt <- BSON.lookup "updatedAt" doc
    return BranchWithEvents {..}
