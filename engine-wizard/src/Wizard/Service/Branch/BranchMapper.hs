module Wizard.Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Service.Event.EventMapper
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState

toDTO :: BranchWithEvents -> Maybe String -> BranchState -> BranchDTO
toDTO branch mForkOfPackageId state =
  BranchDTO
    { _uuid = branch ^. uuid
    , _name = branch ^. name
    , _kmId = branch ^. kmId
    , _state = state
    , _previousPackageId = branch ^. previousPackageId
    , _forkOfPackageId = mForkOfPackageId
    , _ownerUuid = branch ^. ownerUuid
    , _createdAt = branch ^. createdAt
    , _updatedAt = branch ^. updatedAt
    }

toDetailDTO :: BranchWithEvents -> Maybe String -> BranchState -> BranchDetailDTO
toDetailDTO branch mForkOfPackageId state =
  BranchDetailDTO
    { _uuid = branch ^. uuid
    , _name = branch ^. name
    , _kmId = branch ^. kmId
    , _state = state
    , _previousPackageId = branch ^. previousPackageId
    , _forkOfPackageId = mForkOfPackageId
    , _events = toDTOs $ branch ^. events
    , _ownerUuid = branch ^. ownerUuid
    , _createdAt = branch ^. createdAt
    , _updatedAt = branch ^. updatedAt
    }

fromWithEventsDTO :: BranchWithEventsDTO -> BranchWithEvents
fromWithEventsDTO dto =
  BranchWithEvents
    { _uuid = dto ^. uuid
    , _name = dto ^. name
    , _kmId = dto ^. kmId
    , _metamodelVersion = dto ^. metamodelVersion
    , _previousPackageId = dto ^. previousPackageId
    , _events = fromDTOs $ dto ^. events
    , _ownerUuid = dto ^. ownerUuid
    , _createdAt = dto ^. createdAt
    , _updatedAt = dto ^. updatedAt
    }

fromChangeDTO ::
     BranchChangeDTO -> U.UUID -> Int -> Maybe String -> Maybe U.UUID -> UTCTime -> UTCTime -> BranchWithEvents
fromChangeDTO dto bUuid bMetamodelVersion bPackageId mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
    { _uuid = bUuid
    , _name = dto ^. name
    , _kmId = dto ^. kmId
    , _metamodelVersion = bMetamodelVersion
    , _previousPackageId = bPackageId
    , _ownerUuid = mOwnerUuid
    , _events = fromDTOs $ dto ^. events
    , _createdAt = bCreatedAt
    , _updatedAt = bUpdatedAt
    }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> UTCTime -> BranchWithEvents
fromCreateDTO dto bUuid mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
    { _uuid = bUuid
    , _name = dto ^. name
    , _kmId = dto ^. kmId
    , _metamodelVersion = kmMetamodelVersion
    , _previousPackageId = dto ^. previousPackageId
    , _ownerUuid = mOwnerUuid
    , _events = []
    , _createdAt = bCreatedAt
    , _updatedAt = bUpdatedAt
    }
