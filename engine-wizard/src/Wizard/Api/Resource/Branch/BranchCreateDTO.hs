module Wizard.Api.Resource.Branch.BranchCreateDTO where

import GHC.Generics

data BranchCreateDTO =
  BranchCreateDTO
    { _name :: String
    , _kmId :: String
    , _previousPackageId :: Maybe String
    }
  deriving (Generic)
