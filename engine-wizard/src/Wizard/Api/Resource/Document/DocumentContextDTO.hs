module Wizard.Api.Resource.Document.DocumentContextDTO where

import Data.Map
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import qualified Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO as KnowledgeModelDTO
import qualified Wizard.Api.Resource.Level.LevelDTO as LevelDTO
import qualified Wizard.Api.Resource.Package.PackageSimpleDTO as PackageSimpleDTO
import qualified Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO as QuestionnaireReplyDTO
import qualified Wizard.Api.Resource.Report.ReportDTO as ReportDTO
import qualified Wizard.Api.Resource.User.UserDTO as UserDTO
import qualified Wizard.Model.Config.AppConfig as AppConfig

data DocumentContextDTO =
  DocumentContextDTO
    { _uuid :: U.UUID
    , _config :: DocumentContextConfigDTO
    , _questionnaireUuid :: String
    , _questionnaireName :: String
    , _questionnaireReplies :: [QuestionnaireReplyDTO.ReplyDTO]
    , _questionnaireRepliesMap :: Map String QuestionnaireReplyDTO.ReplyDTO
    , _level :: Int
    , _knowledgeModel :: KnowledgeModelDTO.KnowledgeModelDTO
    , _metrics :: [KnowledgeModelDTO.MetricDTO]
    , _levels :: [LevelDTO.LevelDTO]
    , _report :: ReportDTO.ReportDTO
    , _package :: PackageSimpleDTO.PackageSimpleDTO
    , _organization :: AppConfig.AppConfigOrganization
    , _createdBy :: Maybe UserDTO.UserDTO
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DocumentContextDTO where
  a == b =
    _uuid a == _uuid b &&
    _config a == _config b &&
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireReplies a == _questionnaireReplies b &&
    _level a == _level b &&
    _knowledgeModel a == _knowledgeModel b &&
    _metrics a == _metrics b &&
    _levels a == _levels b &&
    _report a == _report b &&
    _package a == _package b && _organization a == _organization b && _createdBy a == _createdBy b

data DocumentContextConfigDTO =
  DocumentContextConfigDTO
    { _levelsEnabled :: Bool
    }
  deriving (Show, Eq, Generic)
