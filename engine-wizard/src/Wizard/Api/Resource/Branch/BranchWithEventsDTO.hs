module Wizard.Api.Resource.Branch.BranchWithEventsDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data BranchWithEventsDTO =
  BranchWithEventsDTO
    { _uuid :: U.UUID
    , _name :: String
    , _kmId :: String
    , _metamodelVersion :: Int
    , _previousPackageId :: Maybe String
    , _forkOfPackageId :: Maybe String
    , _mergeCheckpointPackageId :: Maybe String
    , _events :: [Event]
    , _ownerUuid :: Maybe U.UUID
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
