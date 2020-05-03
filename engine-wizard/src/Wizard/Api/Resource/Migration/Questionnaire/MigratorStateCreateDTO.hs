module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateCreateDTO =
  MigratorStateCreateDTO
    { _targetPackageId :: String
    , _targetTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
