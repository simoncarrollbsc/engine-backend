module Wizard.Api.Resource.Branch.BranchDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Wizard.Model.Branch.BranchState

data BranchDetailDTO =
  BranchDetailDTO
    { _uuid :: U.UUID
    , _name :: String
    , _kmId :: String
    , _state :: BranchState
    , _previousPackageId :: Maybe String
    , _forkOfPackageId :: Maybe String
    , _ownerUuid :: Maybe U.UUID
    , _events :: [Event]
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Generic)
