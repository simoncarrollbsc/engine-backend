module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data BranchChangeDTO =
  BranchChangeDTO
    { _name :: String
    , _kmId :: String
    , _events :: [EventDTO]
    }
  deriving (Generic)
