module Registry.Database.BSON.Package.PackageWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Shared.Database.BSON.Event.Answer ()
import Shared.Database.BSON.Event.Chapter ()
import Shared.Database.BSON.Event.Common
import Shared.Database.BSON.Event.Expert ()
import Shared.Database.BSON.Event.KnowledgeModel ()
import Shared.Database.BSON.Event.Question ()
import Shared.Database.BSON.Event.Reference ()
import Shared.Model.Package.PackageWithEvents

instance ToBSON PackageWithEvents where
  toBSON PackageWithEvents {..} =
    [ "id" BSON.=: _pId
    , "name" BSON.=: _name
    , "organizationId" BSON.=: _organizationId
    , "kmId" BSON.=: _kmId
    , "version" BSON.=: _version
    , "metamodelVersion" BSON.=: _metamodelVersion
    , "description" BSON.=: _description
    , "readme" BSON.=: _readme
    , "license" BSON.=: _license
    , "previousPackageId" BSON.=: _previousPackageId
    , "forkOfPackageId" BSON.=: _forkOfPackageId
    , "mergeCheckpointPackageId" BSON.=: _mergeCheckpointPackageId
    , "events" BSON.=: convertEventToBSON <$> _events
    , "createdAt" BSON.=: _createdAt
    ]
  toBSON' PackageWithEvents {..} =
    [ "id" BSON.=: _pId
    , "name" BSON.=: _name
    , "organizationId" BSON.=: _organizationId
    , "kmId" BSON.=: _kmId
    , "version" BSON.=: _version
    , "metamodelVersion" BSON.=: _metamodelVersion
    , "description" BSON.=: _description
    , "readme" BSON.=: _readme
    , "license" BSON.=: _license
    , "previousPackageId" BSON.=: _previousPackageId
    , "forkOfPackageId" BSON.=: _forkOfPackageId
    , "mergeCheckpointPackageId" BSON.=: _mergeCheckpointPackageId
    , "events" BSON.=: convertEventToBSON <$> _events
    , "createdAt" BSON.=: _createdAt
    ]

instance FromBSON PackageWithEvents where
  fromBSON doc = do
    _pId <- BSON.lookup "id" doc
    _name <- BSON.lookup "name" doc
    _organizationId <- BSON.lookup "organizationId" doc
    _kmId <- BSON.lookup "kmId" doc
    _version <- BSON.lookup "version" doc
    _metamodelVersion <- BSON.lookup "metamodelVersion" doc
    _description <- BSON.lookup "description" doc
    _readme <- BSON.lookup "readme" doc
    _license <- BSON.lookup "license" doc
    _previousPackageId <- BSON.lookup "previousPackageId" doc
    _forkOfPackageId <- BSON.lookup "forkOfPackageId" doc
    _mergeCheckpointPackageId <- BSON.lookup "mergeCheckpointPackageId" doc
    pkgEventsSerialized <- BSON.lookup "events" doc
    let _events = (fromJust . chooseEventDeserializator) <$> pkgEventsSerialized
    _createdAt <- BSON.lookup "createdAt" doc
    return PackageWithEvents {..}
  fromBSON' doc = do
    _pId <- BSON.lookup "id" doc
    _name <- BSON.lookup "name" doc
    _organizationId <- BSON.lookup "organizationId" doc
    _kmId <- BSON.lookup "kmId" doc
    _version <- BSON.lookup "version" doc
    _metamodelVersion <- BSON.lookup "metamodelVersion" doc
    _description <- BSON.lookup "description" doc
    _readme <- BSON.lookup "readme" doc
    _license <- BSON.lookup "license" doc
    _previousPackageId <- BSON.lookup "previousPackageId" doc
    _forkOfPackageId <- BSON.lookup "forkOfPackageId" doc
    _mergeCheckpointPackageId <- BSON.lookup "mergeCheckpointPackageId" doc
    pkgEventsSerialized <- BSON.lookup "events" doc
    let _events = (fromJust . chooseEventDeserializator) <$> pkgEventsSerialized
    _createdAt <- BSON.lookup "createdAt" doc
    return PackageWithEvents {..}
