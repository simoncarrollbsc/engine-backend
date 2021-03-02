module Wizard.Service.Document.DocumentContextService
  ( createDocumentContext
  ) where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.User.UserService

createDocumentContext :: Document -> AppContextM DocumentContextDTO
createDocumentContext doc = do
  qtn <- findQuestionnaireById . U.toString $ doc ^. questionnaireUuid
  pkg <- getPackageById (qtn ^. packageId)
  metrics <- findMetrics
  ls <- findLevels
  km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  mCreatedBy <- forM (fmap U.toString (qtn ^. creatorUuid)) getUserById
  appConfig <- getAppConfig
  serverConfig <- asks _appContextServerConfig
  let org = appConfig ^. organization
  dmpUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  qtnCtn <- compileQuestionnaire qtn
  let _level =
        if appConfig ^. questionnaire . levels . enabled
          then qtnCtn ^. level
          else 9999
  report <- generateReport _level metrics km (M.toList $ qtnCtn ^. replies)
  let qtnVersion =
        case (doc ^. questionnaireEventUuid) of
          (Just eventUuid) -> findQuestionnaireVersionUuid eventUuid (qtn ^. versions)
          _ -> Nothing
  qtnVersionDtos <- traverse enhanceQuestionnaireVersion (qtn ^. versions)
  return $
    toDocumentContextDTO
      dmpUuid
      appConfig
      serverConfig
      qtn
      qtnCtn
      qtnVersion
      qtnVersionDtos
      _level
      km
      metrics
      ls
      report
      pkg
      org
      mCreatedBy
      now

-- --------------------------------
-- PRIVATE
-- --------------------------------
findQuestionnaireVersionUuid :: U.UUID -> [QuestionnaireVersion] -> Maybe U.UUID
findQuestionnaireVersionUuid _ [] = Nothing
findQuestionnaireVersionUuid desiredEventUuid (version:rest)
  | desiredEventUuid == (version ^. eventUuid) = Just $ version ^. uuid
  | otherwise = findQuestionnaireVersionUuid desiredEventUuid rest
