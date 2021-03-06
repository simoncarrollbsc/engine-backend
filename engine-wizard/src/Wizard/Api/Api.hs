module Wizard.Api.Api where

import Servant

import Wizard.Api.Handler.ActionKey.Api
import Wizard.Api.Handler.Auth.Api
import Wizard.Api.Handler.BookReference.Api
import Wizard.Api.Handler.Branch.Api
import Wizard.Api.Handler.Cache.Api
import Wizard.Api.Handler.Config.Api
import Wizard.Api.Handler.Document.Api
import Wizard.Api.Handler.Feedback.Api
import Wizard.Api.Handler.Info.Api
import Wizard.Api.Handler.KnowledgeModel.Api
import Wizard.Api.Handler.Level.Api
import Wizard.Api.Handler.Metric.Api
import Wizard.Api.Handler.Migration.Api
import Wizard.Api.Handler.Package.Api
import Wizard.Api.Handler.Questionnaire.Api
import Wizard.Api.Handler.Registry.Api
import Wizard.Api.Handler.Submission.Api
import Wizard.Api.Handler.Template.Api
import Wizard.Api.Handler.Token.Api
import Wizard.Api.Handler.Typehint.Api
import Wizard.Api.Handler.User.Api
import Wizard.Api.Handler.Version.Api
import Wizard.Model.Context.BaseContext

type AppAPI
   = ActionKeyAPI
     :<|> AuthAPI
     :<|> BookReferenceAPI
     :<|> BranchAPI
     :<|> CacheAPI
     :<|> ConfigAPI
     :<|> DocumentAPI
     :<|> FeedbackAPI
     :<|> InfoAPI
     :<|> KnowledgeModelAPI
     :<|> LevelAPI
     :<|> MetricAPI
     :<|> MigrationAPI
     :<|> PackageAPI
     :<|> QuestionnaireAPI
     :<|> RegistryAPI
     :<|> SubmissionAPI
     :<|> TemplateAPI
     :<|> TokenAPI
     :<|> TypehintAPI
     :<|> UserAPI
     :<|> VersionAPI

appApi :: Proxy AppAPI
appApi = Proxy

appServer :: ServerT AppAPI BaseContextM
appServer =
  actionKeyServer :<|> authServer :<|> bookReferenceServer :<|> branchServer :<|> cacheServer :<|> configServer :<|>
  documentServer :<|>
  feedbackServer :<|>
  infoServer :<|>
  knowledgeModelServer :<|>
  levelServer :<|>
  metricServer :<|>
  migrationServer :<|>
  packageServer :<|>
  questionnaireServer :<|>
  registryServer :<|>
  submissionServer :<|>
  templateServer :<|>
  tokenServer :<|>
  typehintServer :<|>
  userServer :<|>
  versionServer
