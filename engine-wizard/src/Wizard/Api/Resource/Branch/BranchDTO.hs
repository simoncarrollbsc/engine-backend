module Wizard.Api.Resource.Branch.BranchDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Branch.BranchState

data BranchDTO =
  BranchDTO
    { _uuid :: U.UUID
    , _name :: String
    , _kmId :: String
    , _state :: BranchState
    , _previousPackageId :: Maybe String
    , _forkOfPackageId :: Maybe String
    , _ownerUuid :: Maybe U.UUID
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)
