module Wizard.Service.Document.DocumentContextMapper where

import Control.Lens ((^.))
import Data.Map as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Level.LevelMapper
import Wizard.Service.Metric.MetricMapper
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import Wizard.Service.Report.ReportMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContextDTO :: DocumentContext -> DocumentContextDTO
toDocumentContextDTO dc =
  DocumentContextDTO
    { _uuid = dc ^. uuid
    , _config = toDocumentContextConfigDTO $ dc ^. config
    , _questionnaireUuid = dc ^. questionnaireUuid
    , _questionnaireName = dc ^. questionnaireName
    , _questionnaireReplies = replies
    , _questionnaireRepliesMap = M.fromList $ (\reply -> (reply ^. path, reply)) <$> replies
    , _level = dc ^. level
    , _e = toKnowledgeModelDTO $ dc ^. knowledgeModel
    , _metrics = toMetricDTO <$> dc ^. metrics
    , _levels = toLevelDTO <$> dc ^. levels
    , _report = toReportDTO $ dc ^. report
    , _package = toSimpleDTO (dc ^. package)
    , _organization = dc ^. organization
    , _createdBy = USR_Mapper.toDTO <$> dc ^. createdBy
    , _createdAt = dc ^. createdAt
    , _updatedAt = dc ^. updatedAt
    }
  where
    replies = QTN_Mapper.toReplyDTO <$> dc ^. questionnaireReplies

toDocumentContextConfigDTO :: DocumentContextConfig -> DocumentContextConfigDTO
toDocumentContextConfigDTO config = DocumentContextConfigDTO {_levelsEnabled = config ^. levelsEnabled}

fromCreateContextDTO ::
     U.UUID
  -> AppConfig
  -> Questionnaire
  -> Int
  -> KnowledgeModel
  -> [Metric]
  -> [Level]
  -> Report
  -> Package
  -> AppConfigOrganization
  -> Maybe User
  -> UTCTime
  -> DocumentContext
fromCreateContextDTO dmpUuid appConfig qtn level km metrics ls report pkg org mCreatedBy now =
  DocumentContext
    { _uuid = dmpUuid
    , _config = DocumentContextConfig {_levelsEnabled = appConfig ^. questionnaire . levels . enabled}
    , _questionnaireUuid = U.toString $ qtn ^. uuid
    , _questionnaireName = qtn ^. name
    , _questionnaireReplies = qtn ^. replies
    , _level = level
    , _e = km
    , _metrics = metrics
    , _levels = ls
    , _report = report
    , _package = pkg
    , _organization = org
    , _createdBy = mCreatedBy
    , _createdAt = now
    , _updatedAt = now
    }
