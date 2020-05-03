module Wizard.Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Service.Event.EventMapper
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper

toDTO :: MigratorState -> MigratorStateDTO
toDTO ms =
  MigratorStateDTO
    { _uuid = ms ^. branchUuid
    , _migrationState = toMigrationStateDTO $ ms ^. migrationState
    , _previousPackageId = ms ^. branchPreviousPackageId
    , _targetPackageId = ms ^. targetPackageId
    , _currentKnowledgeModel = toKnowledgeModelDTO <$> ms ^. currentKnowledgeModel
    }

toMigrationStateDTO :: MigrationState -> MigrationStateDTO
toMigrationStateDTO RunningState = RunningStateDTO
toMigrationStateDTO (ConflictState conflict) = ConflictStateDTO (toConflictDTO conflict)
toMigrationStateDTO ErrorState = ErrorStateDTO
toMigrationStateDTO CompletedState = CompletedStateDTO

toConflictDTO :: Conflict -> ConflictDTO
toConflictDTO (CorrectorConflict event) = CorrectorConflictDTO (toDTOFn event)

fromDetailDTO :: MigratorStateDetailDTO -> MigratorState
fromDetailDTO dto =
  MigratorState
    { _branchUuid = dto ^. branchUuid
    , _metamodelVersion = dto ^. metamodelVersion
    , _migrationState = fromMigrationStateDTO $ dto ^. migrationState
    , _branchPreviousPackageId = dto ^. branchPreviousPackageId
    , _targetPackageId = dto ^. targetPackageId
    , _branchEvents = fromDTOs (dto ^. branchEvents)
    , _targetPackageEvents = fromDTOs (dto ^. targetPackageEvents)
    , _resultEvents = fromDTOs (dto ^. resultEvents)
    , _currentKnowledgeModel = fromKnowledgeModelDTO <$> dto ^. currentKnowledgeModel
    }

fromMigrationStateDTO :: MigrationStateDTO -> MigrationState
fromMigrationStateDTO RunningStateDTO = RunningState
fromMigrationStateDTO (ConflictStateDTO conflict) = ConflictState (fromConflictDTO conflict)
fromMigrationStateDTO ErrorStateDTO = ErrorState
fromMigrationStateDTO CompletedStateDTO = CompletedState

fromConflictDTO :: ConflictDTO -> Conflict
fromConflictDTO (CorrectorConflictDTO event) = CorrectorConflict (fromDTOFn event)

fromCreateDTO :: BranchWithEvents -> Package -> [Event] -> String -> [Event] -> KnowledgeModel -> MigratorState
fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km =
  MigratorState
    { _branchUuid = branch ^. uuid
    , _metamodelVersion = kmMetamodelVersion
    , _migrationState = RunningState
    , _branchPreviousPackageId = previousPkg ^. pId
    , _targetPackageId = targetPkgId
    , _branchEvents = branchEvents
    , _targetPackageEvents = targetPkgEvents
    , _resultEvents = []
    , _currentKnowledgeModel = Just km
    }
