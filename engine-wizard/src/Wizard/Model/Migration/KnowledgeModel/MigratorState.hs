module Wizard.Model.Migration.KnowledgeModel.MigratorState where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel

data MigrationState
  = RunningState
  | ConflictState Conflict
  | ErrorState
  | CompletedState
  deriving (Show, Eq, Generic)

data Conflict =
  CorrectorConflict Event
  deriving (Show, Eq, Generic)

data MigrationConflictAction
  = MCAApply
  | MCAEdited
  | MCAReject
  deriving (Show, Eq, Generic)

data MigratorState =
  MigratorState
    { _branchUuid :: U.UUID
    , _metamodelVersion :: Int
    , _migrationState :: MigrationState
    , _branchPreviousPackageId :: String
    , _targetPackageId :: String
    , _branchEvents :: [Event]
    , _targetPackageEvents :: [Event]
    , _resultEvents :: [Event]
    , _currentKnowledgeModel :: Maybe KnowledgeModel
    }
  deriving (Show, Eq)
