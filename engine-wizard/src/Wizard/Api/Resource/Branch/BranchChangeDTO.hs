module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

import Shared.Model.Event.Event

data BranchChangeDTO =
  BranchChangeDTO
    { _name :: String
    , _kmId :: String
    , _events :: [Event]
    }
  deriving (Generic)
