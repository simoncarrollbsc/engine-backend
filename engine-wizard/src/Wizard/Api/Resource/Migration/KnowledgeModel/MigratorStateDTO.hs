module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO

data MigratorStateDTO =
  MigratorStateDTO
    { _uuid :: U.UUID
    , _migrationState :: MigrationStateDTO
    , _previousPackageId :: String
    , _targetPackageId :: String
    , _currentKnowledgeModel :: Maybe KnowledgeModel
    }
  deriving (Show, Eq, Generic)
