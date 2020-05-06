module Wizard.Database.BSON.Package.PackageWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Event.Event ()
import Shared.Model.Package.PackageWithEvents

instance ToBSON PackageWithEvents where
  toBSON PackageWithEvents {..} =
    [ "id" BSON.=: _packageWithEventsPId
    , "name" BSON.=: _packageWithEventsName
    , "organizationId" BSON.=: _packageWithEventsOrganizationId
    , "kmId" BSON.=: _packageWithEventsKmId
    , "version" BSON.=: _packageWithEventsVersion
    , "metamodelVersion" BSON.=: _packageWithEventsMetamodelVersion
    , "description" BSON.=: _packageWithEventsDescription
    , "readme" BSON.=: _packageWithEventsReadme
    , "license" BSON.=: _packageWithEventsLicense
    , "previousPackageId" BSON.=: _packageWithEventsPreviousPackageId
    , "forkOfPackageId" BSON.=: _packageWithEventsForkOfPackageId
    , "mergeCheckpointPackageId" BSON.=: _packageWithEventsMergeCheckpointPackageId
    , "events" BSON.=: _packageWithEventsEvents
    , "createdAt" BSON.=: _packageWithEventsCreatedAt
    , "_co" BSON.=: "Package"
    ]

instance FromBSON PackageWithEvents where
  fromBSON doc = do
    _packageWithEventsPId <- BSON.lookup "id" doc
    _packageWithEventsName <- BSON.lookup "name" doc
    _packageWithEventsOrganizationId <- BSON.lookup "organizationId" doc
    _packageWithEventsKmId <- BSON.lookup "kmId" doc
    _packageWithEventsVersion <- BSON.lookup "version" doc
    _packageWithEventsMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _packageWithEventsDescription <- BSON.lookup "description" doc
    _packageWithEventsReadme <- BSON.lookup "readme" doc
    _packageWithEventsLicense <- BSON.lookup "license" doc
    _packageWithEventsPreviousPackageId <- BSON.lookup "previousPackageId" doc
    _packageWithEventsForkOfPackageId <- BSON.lookup "forkOfPackageId" doc
    _packageWithEventsMergeCheckpointPackageId <- BSON.lookup "mergeCheckpointPackageId" doc
    _packageWithEventsEvents <- BSON.lookup "events" doc
    _packageWithEventsCreatedAt <- BSON.lookup "createdAt" doc
    return PackageWithEvents {..}
