module Wizard.Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState

amsterdamBranch :: BranchDTO
amsterdamBranch =
  BranchDTO
    { _uuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
    , _name = amsterdamPackage ^. name
    , _kmId = amsterdamPackage ^. kmId
    , _previousPackageId = Just $ netherlandsPackage ^. pId
    , _forkOfPackageId = Just $ netherlandsPackage ^. pId
    , _state = BSEdited
    , _ownerUuid = Just $ userAlbert ^. uuid
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

amsterdamBranchWithEvents :: BranchWithEvents
amsterdamBranchWithEvents =
  BranchWithEvents
    { _uuid = amsterdamBranch ^. uuid
    , _name = amsterdamBranch ^. name
    , _kmId = amsterdamBranch ^. kmId
    , _metamodelVersion = kmMetamodelVersion
    , _previousPackageId = amsterdamBranch ^. previousPackageId
    , _ownerUuid = amsterdamBranch ^. ownerUuid
    , _events =
        [ AddQuestionEvent' a_km1_ch1_q1'
        , AddQuestionEvent' a_km1_ch1_q2'
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1'
        , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
        , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2'
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
        , AddExpertEvent' a_km1_ch1_q2_eAlbert
        , AddExpertEvent' a_km1_ch1_q2_eNikola
        , AddReferenceEvent' a_km1_ch1_q2_rCh1'
        , AddReferenceEvent' a_km1_ch1_q2_rCh2'
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3'
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
    , _createdAt = amsterdamBranch ^. createdAt
    , _updatedAt = amsterdamBranch ^. updatedAt
    }

amsterdamBranchCreate :: BranchCreateDTO
amsterdamBranchCreate =
  BranchCreateDTO
    { _name = amsterdamBranch ^. name
    , _kmId = amsterdamBranch ^. kmId
    , _previousPackageId = amsterdamBranch ^. previousPackageId
    }

amsterdamBranchChange :: BranchChangeDTO
amsterdamBranchChange =
  BranchChangeDTO {_name = "EDITED: " ++ amsterdamBranch ^. name, _kmId = amsterdamBranch ^. kmId, _events = []}

amsterdamBranchDetail :: BranchDetailDTO
amsterdamBranchDetail =
  BranchDetailDTO
    { _uuid = amsterdamBranch ^. uuid
    , _name = amsterdamBranch ^. name
    , _kmId = amsterdamBranch ^. kmId
    , _state = BSEdited
    , _previousPackageId = amsterdamBranch ^. previousPackageId
    , _forkOfPackageId = amsterdamBranch ^. forkOfPackageId
    , _ownerUuid = amsterdamBranch ^. ownerUuid
    , _events = toDTOs $ amsterdamBranchWithEvents ^. events
    , _createdAt = amsterdamBranch ^. createdAt
    , _updatedAt = amsterdamBranch ^. updatedAt
    }

leidenBranch :: BranchDTO
leidenBranch =
  BranchDTO
    { _uuid = fromJust (U.fromString "47421955-ba30-48d4-8c49-9ec47eda2cad")
    , _name = "Leiden KM"
    , _kmId = "leiden-km"
    , _state = BSDefault
    , _previousPackageId = Just $ netherlandsPackage ^. pId
    , _forkOfPackageId = Just $ netherlandsPackage ^. pId
    , _ownerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

leidenBranchCreate :: BranchCreateDTO
leidenBranchCreate =
  BranchCreateDTO
    {_name = leidenBranch ^. name, _kmId = leidenBranch ^. kmId, _previousPackageId = leidenBranch ^. previousPackageId}
