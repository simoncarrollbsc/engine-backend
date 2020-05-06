module Wizard.Database.BSON.Branch.BranchWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.Event ()
import Wizard.Model.Branch.Branch

instance ToBSON BranchWithEvents where
  toBSON BranchWithEvents {..} =
    [ "uuid" BSON.=: _branchWithEventsUuid
    , "name" BSON.=: _branchWithEventsName
    , "kmId" BSON.=: _branchWithEventsKmId
    , "metamodelVersion" BSON.=: _branchWithEventsMetamodelVersion
    , "previousPackageId" BSON.=: _branchWithEventsPreviousPackageId
    , "events" BSON.=: _branchWithEventsEvents
    , "ownerUuid" BSON.=: _branchWithEventsOwnerUuid
    , "createdAt" BSON.=: _branchWithEventsCreatedAt
    , "updatedAt" BSON.=: _branchWithEventsUpdatedAt
    , "_co" BSON.=: "Branch"
    ]

instance FromBSON BranchWithEvents where
  fromBSON doc = do
    _branchWithEventsUuid <- BSON.lookup "uuid" doc
    _branchWithEventsName <- BSON.lookup "name" doc
    _branchWithEventsKmId <- BSON.lookup "kmId" doc
    _branchWithEventsMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _branchWithEventsPreviousPackageId <- BSON.lookup "previousPackageId" doc
    _branchWithEventsEvents <- BSON.lookup "events" doc
    _branchWithEventsOwnerUuid <- BSON.lookup "ownerUuid" doc
    _branchWithEventsCreatedAt <- BSON.lookup "createdAt" doc
    _branchWithEventsUpdatedAt <- BSON.lookup "updatedAt" doc
    return BranchWithEvents {..}
