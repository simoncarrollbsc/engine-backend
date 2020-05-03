module Shared.Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper

globalPackageEmpty :: PackageWithEvents
globalPackageEmpty =
  PackageWithEvents
    { _pId = "global:core:0.0.1"
    , _name = "Global Knowledge Model"
    , _organizationId = "global"
    , _kmId = "core"
    , _version = "0.0.1"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "Empty package"
    , _readme = "# Global Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Nothing
    , _forkOfPackageId = Nothing
    , _mergeCheckpointPackageId = Nothing
    , _events = [AddQuestionEvent' a_km1_ch1_q1']
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalPackage :: PackageWithEvents
globalPackage =
  PackageWithEvents
    { _pId = "global:core:1.0.0"
    , _name = "Global Knowledge Model"
    , _organizationId = "global"
    , _kmId = "core"
    , _version = "1.0.0"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "First Release"
    , _readme = "# Global Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Nothing
    , _forkOfPackageId = Nothing
    , _mergeCheckpointPackageId = Nothing
    , _events =
        [ AddKnowledgeModelEvent' a_km1
        , AddTagEvent' a_km1_tds
        , AddTagEvent' a_km1_tbi
        , AddIntegrationEvent' a_km1_iop
        , AddIntegrationEvent' a_km1_ibp
        ]
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalPackageDto :: PackageDTO
globalPackageDto = toDTO globalPackage

netherlandsPackage :: PackageWithEvents
netherlandsPackage =
  PackageWithEvents
    { _pId = "org.nl:core-nl:1.0.0"
    , _name = "Netherlands Knowledge Model"
    , _organizationId = "org.nl"
    , _kmId = "core-nl"
    , _version = "1.0.0"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "First Release"
    , _readme = "# Netherlands Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Just $ globalPackage ^. pId
    , _forkOfPackageId = Just $ globalPackage ^. pId
    , _mergeCheckpointPackageId = Just $ globalPackage ^. pId
    , _events = [AddChapterEvent' a_km1_ch1]
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

netherlandsPackageV2 :: PackageWithEvents
netherlandsPackageV2 =
  PackageWithEvents
    { _pId = "org.nl:core-nl:2.0.0"
    , _name = "Netherlands Knowledge Model"
    , _organizationId = "org.nl"
    , _kmId = "core-nl"
    , _version = "2.0.0"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "Second Release"
    , _readme = "# Netherlands Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Just $ netherlandsPackage ^. pId
    , _forkOfPackageId = Just $ globalPackage ^. pId
    , _mergeCheckpointPackageId = Just $ globalPackage ^. pId
    , _events = [AddChapterEvent' a_km1_ch4]
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

amsterdamPackage :: PackageWithEvents
amsterdamPackage =
  PackageWithEvents
    { _pId = "org.nl.amsterdam:core-amsterdam:1.0.0"
    , _name = "Amsterdam Knowledge Model"
    , _organizationId = "org.nl.amsterdam"
    , _kmId = "core-amsterdam"
    , _version = "1.0.0"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "First Release"
    , _readme = "# Amsterdam Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Just $ netherlandsPackage ^. pId
    , _forkOfPackageId = Just $ netherlandsPackage ^. pId
    , _mergeCheckpointPackageId = Just $ netherlandsPackage ^. pId
    , _events = []
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

germanyPackage :: PackageWithEvents
germanyPackage =
  PackageWithEvents
    { _pId = "org.de:core-de:1.0.0"
    , _name = "Germany Knowledge Model"
    , _organizationId = "org.de"
    , _kmId = "core-de"
    , _version = "1.0.0"
    , _metamodelVersion = kmMetamodelVersion
    , _description = "First Release"
    , _readme = "# Germany Knowledge Model"
    , _license = "Apache-2.0"
    , _previousPackageId = Just $ globalPackageEmpty ^. pId
    , _forkOfPackageId = Just $ globalPackageEmpty ^. pId
    , _mergeCheckpointPackageId = Just $ globalPackageEmpty ^. pId
    , _events =
        [ AddKnowledgeModelEvent' a_km1
        , AddTagEvent' a_km1_tds
        , AddTagEvent' a_km1_tbi
        , AddIntegrationEvent' a_km1_iop
        , AddIntegrationEvent' a_km1_ibp
        , AddChapterEvent' a_km1_ch1
        , AddQuestionEvent' a_km1_ch1_q1'
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
        , AddQuestionEvent' a_km1_ch2_q4'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q5'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q7'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q8'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6'
        , AddAnswerEvent' a_km1_ch2_q4_it_q6_aNo
        , AddAnswerEvent' a_km1_ch2_q4_it_q6_aYes
        , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4'
        , AddQuestionEvent' a_km1_ch2_ansYes6_fuq5'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2'
        , AddExpertEvent' a_km1_ch2_q6_eAlbert
        , AddExpertEvent' a_km1_ch2_q6_eNikola
        , AddReferenceEvent' a_km1_ch2_q6_rCh1'
        , AddReferenceEvent' a_km1_ch2_q6_rCh2'
        , AddChapterEvent' a_km1_ch3
        , AddQuestionEvent' a_km1_ch3_q9'
        , AddQuestionEvent' a_km1_ch3_q10'
        ]
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
