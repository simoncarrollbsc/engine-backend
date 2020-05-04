module Wizard.Database.BSON.Migration.KnowledgeModel.MigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import LensesConfig
import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.Common ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Migration.KnowledgeModel.MigratorState

instance ToBSON MigrationState where
  toBSON RunningState = ["stateType" BSON.=: "RunningState"]
  toBSON (ConflictState (CorrectorConflict event)) =
    ["stateType" BSON.=: "ConflictState", "targetEvent" BSON.=: toBSON event]
  toBSON ErrorState = ["stateType" BSON.=: "ErrorState"]
  toBSON CompletedState = ["stateType" BSON.=: "CompletedState"]
  toBSON' RunningState = ["stateType" BSON.=: "RunningState"]
  toBSON' (ConflictState (CorrectorConflict event)) =
    ["stateType" BSON.=: "ConflictState", "targetEvent" BSON.=: toBSON event]
  toBSON' ErrorState = ["stateType" BSON.=: "ErrorState"]
  toBSON' CompletedState = ["stateType" BSON.=: "CompletedState"]

instance FromBSON MigrationState where
  fromBSON doc = do
    stateType <- BSON.lookup "stateType" doc
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> do
        event <- BSON.lookup "targetEvent" doc
        return . ConflictState . CorrectorConflict . fromJust . fromBSON $ event
      "ErrorState" -> return ErrorState
      "CompletedState" -> return CompletedState
  fromBSON' doc = do
    stateType <- BSON.lookup "stateType" doc
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> do
        event <- BSON.lookup "targetEvent" doc
        return . ConflictState . CorrectorConflict . fromJust . fromBSON $ event
      "ErrorState" -> return ErrorState
      "CompletedState" -> return CompletedState

instance ToBSON MigratorState where
  toBSON ms =
    [ "branchUuid" BSON.=: (ms ^. branchUuid)
    , "metamodelVersion" BSON.=: (ms ^. metamodelVersion)
    , "migrationState" BSON.=: (ms ^. migrationState)
    , "branchPreviousPackageId" BSON.=: (ms ^. branchPreviousPackageId)
    , "targetPackageId" BSON.=: (ms ^. targetPackageId)
    , "branchEvents" BSON.=: toBSON <$> (ms ^. branchEvents)
    , "targetPackageEvents" BSON.=: toBSON <$> (ms ^. targetPackageEvents)
    , "resultEvents" BSON.=: toBSON <$> (ms ^. resultEvents)
    , "currentKnowledgeModel" BSON.=: (Nothing :: Maybe KnowledgeModel)
    ]
  toBSON' ms =
    [ "branchUuid" BSON.=: (ms ^. branchUuid)
    , "metamodelVersion" BSON.=: (ms ^. metamodelVersion)
    , "migrationState" BSON.=: (ms ^. migrationState)
    , "branchPreviousPackageId" BSON.=: (ms ^. branchPreviousPackageId)
    , "targetPackageId" BSON.=: (ms ^. targetPackageId)
    , "branchEvents" BSON.=: toBSON <$> (ms ^. branchEvents)
    , "targetPackageEvents" BSON.=: toBSON <$> (ms ^. targetPackageEvents)
    , "resultEvents" BSON.=: toBSON <$> (ms ^. resultEvents)
    , "currentKnowledgeModel" BSON.=: (Nothing :: Maybe KnowledgeModel)
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    msBranchUuid <- BSON.lookup "branchUuid" doc
    msMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    msMigrationState <- BSON.lookup "migrationState" doc
    msBranchPreviousPackageId <- BSON.lookup "branchPreviousPackageId" doc
    msTargetPackageId <- BSON.lookup "targetPackageId" doc
    msBranchEventsSerialized <- BSON.lookup "branchEvents" doc
    let msBranchEvents = fmap (fromJust . fromBSON) msBranchEventsSerialized
    msTargetPackageEventsSerialized <- BSON.lookup "targetPackageEvents" doc
    let msTargetPackageEvents = fmap (fromJust . fromBSON) msTargetPackageEventsSerialized
    msResultEventsSerialized <- BSON.lookup "resultEvents" doc
    let msResultEvents = fmap (fromJust . fromBSON) msResultEventsSerialized
    return
      MigratorState
        { _branchUuid = msBranchUuid
        , _metamodelVersion = msMetamodelVersion
        , _migrationState = msMigrationState
        , _branchPreviousPackageId = msBranchPreviousPackageId
        , _targetPackageId = msTargetPackageId
        , _branchEvents = msBranchEvents
        , _targetPackageEvents = msTargetPackageEvents
        , _resultEvents = msResultEvents
        , _currentKnowledgeModel = Nothing
        }
  fromBSON' doc = do
    msBranchUuid <- BSON.lookup "branchUuid" doc
    msMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    msMigrationState <- BSON.lookup "migrationState" doc
    msBranchPreviousPackageId <- BSON.lookup "branchPreviousPackageId" doc
    msTargetPackageId <- BSON.lookup "targetPackageId" doc
    msBranchEventsSerialized <- BSON.lookup "branchEvents" doc
    let msBranchEvents = fmap (fromJust . fromBSON) msBranchEventsSerialized
    msTargetPackageEventsSerialized <- BSON.lookup "targetPackageEvents" doc
    let msTargetPackageEvents = fmap (fromJust . fromBSON) msTargetPackageEventsSerialized
    msResultEventsSerialized <- BSON.lookup "resultEvents" doc
    let msResultEvents = fmap (fromJust . fromBSON) msResultEventsSerialized
    return
      MigratorState
        { _branchUuid = msBranchUuid
        , _metamodelVersion = msMetamodelVersion
        , _migrationState = msMigrationState
        , _branchPreviousPackageId = msBranchPreviousPackageId
        , _targetPackageId = msTargetPackageId
        , _branchEvents = msBranchEvents
        , _targetPackageEvents = msTargetPackageEvents
        , _resultEvents = msResultEvents
        , _currentKnowledgeModel = Nothing
        }
