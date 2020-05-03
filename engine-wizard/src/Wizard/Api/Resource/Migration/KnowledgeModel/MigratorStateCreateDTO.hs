module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO where

import GHC.Generics

data MigratorStateCreateDTO =
  MigratorStateCreateDTO
    { _targetPackageId :: String
    }
  deriving (Show, Eq, Generic)
