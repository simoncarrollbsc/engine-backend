module LensesConfig where

import Control.Lens (makeFields, makeFieldsNoPrefix)

import Registry.Api.Resource.Organization.OrganizationDTO
import qualified Registry.Model.ActionKey.ActionKey as Registry_ActionKey
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Level.LevelDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO as KM_MigratorConflictDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO as KM_createDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO as KM_MigratorStateDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO as KM_MigratorStateDetailDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO as QTN_changeDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO as QTN_createDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO as QTN_MigratorStateDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO
import Wizard.Integration.Resource.Package.PackageDetailIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Branch.Branch
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentTemplateContext
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Level.Level
import qualified Wizard.Model.Migration.KnowledgeModel.MigratorState as KM_MigratorState
import qualified Wizard.Model.Migration.Questionnaire.MigratorState as QTN_MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Model.Template.Template
import Wizard.Model.User.User

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFieldsNoPrefix ''ActionKey

-- Model / BookReference
makeFieldsNoPrefix ''BookReference

-- Model / Branch
makeFieldsNoPrefix ''Branch

makeFieldsNoPrefix ''BranchWithEvents

-- Model / Config
makeFieldsNoPrefix ''AppConfig

makeFieldsNoPrefix ''AppConfigOrganization

makeFieldsNoPrefix ''AppConfigAuth

makeFieldsNoPrefix ''AppConfigAuthInternal

makeFieldsNoPrefix ''AppConfigAuthExternal

makeFieldsNoPrefix ''AppConfigAuthExternalService

makeFieldsNoPrefix ''AppConfigAuthExternalServiceParameter

makeFieldsNoPrefix ''AppConfigAuthExternalServiceStyle

makeFieldsNoPrefix ''AppConfigPrivacyAndSupport

makeFieldsNoPrefix ''AppConfigDashboard

makeFieldsNoPrefix ''AppConfigDashboardWidgets

makeFieldsNoPrefix ''AppConfigLookAndFeel

makeFieldsNoPrefix ''AppConfigLookAndFeelCustomMenuLink

makeFieldsNoPrefix ''AppConfigRegistry

makeFieldsNoPrefix ''AppConfigQuestionnaire

makeFieldsNoPrefix ''AppConfigQuestionnaireFeedback

makeFieldsNoPrefix ''AppConfigTemplate

makeFieldsNoPrefix ''AppConfigSubmission

makeFieldsNoPrefix ''AppConfigSubmissionService

makeFieldsNoPrefix ''AppConfigSubmissionServiceSupportedFormat

makeFieldsNoPrefix ''AppConfigSubmissionServiceRequest

makeFieldsNoPrefix ''AppConfigSubmissionServiceRequestMultipart

makeFieldsNoPrefix ''SimpleFeature

makeFieldsNoPrefix ''ServerConfig

makeFieldsNoPrefix ''ServerConfigGeneral

makeFieldsNoPrefix ''ServerConfigDatabase

makeFieldsNoPrefix ''ServerConfigMessaging

makeFieldsNoPrefix ''ServerConfigJwt

makeFieldsNoPrefix ''ServerConfigRoles

makeFieldsNoPrefix ''ServerConfigMail

makeFieldsNoPrefix ''ServerConfigRegistry

makeFieldsNoPrefix ''ServerConfigAnalytics

makeFieldsNoPrefix ''ServerConfigFeedback

makeFieldsNoPrefix ''BuildInfoConfig

-- Model / Context
makeFieldsNoPrefix ''BaseContext

makeFieldsNoPrefix ''AppContext

-- Model / Document
makeFieldsNoPrefix ''Document

makeFieldsNoPrefix ''DocumentMetadata

makeFieldsNoPrefix ''DocumentContext

makeFieldsNoPrefix ''DocumentContextConfig

makeFieldsNoPrefix ''DocumentTemplateContext

-- Model / Event
makeFields ''Event

makeFields ''EventField

-- Model / Feedback
makeFieldsNoPrefix ''Feedback

-- Model / Http
makeFieldsNoPrefix ''HttpRequest

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''KnowledgeModelEntities

makeFields ''Chapter

makeFields ''Question

makeFields ''OptionsQuestion

makeFields ''ListQuestion

makeFields ''ValueQuestion

makeFields ''IntegrationQuestion

makeFields ''Answer

makeFields ''Expert

makeFields ''Reference

makeFields ''ResourcePageReference

makeFields ''URLReference

makeFields ''CrossReference

makeFields ''Metric

makeFields ''MetricMeasure

makeFields ''Tag

makeFields ''Integration

-- Model / Level
makeFieldsNoPrefix ''Level

-- Model / Migration / KnowledgeModel
makeFieldsNoPrefix ''KM_MigratorState.MigratorState

-- Model / Migration / Questionnaire
makeFieldsNoPrefix ''QTN_MigratorState.MigratorState

-- Model / Package
makeFieldsNoPrefix ''Package

makeFieldsNoPrefix ''PackageWithEvents

-- Model / PackageBundle
makeFieldsNoPrefix ''PackageBundle

-- Model / Questionnaire
makeFieldsNoPrefix ''Questionnaire

makeFieldsNoPrefix ''Reply

makeFieldsNoPrefix ''ReplyValue

makeFieldsNoPrefix ''IntegrationReplyValue

makeFieldsNoPrefix ''Label

-- Model / Report
makeFieldsNoPrefix ''Indication

makeFieldsNoPrefix ''MetricSummary

makeFieldsNoPrefix ''ChapterReport

makeFieldsNoPrefix ''TotalReport

makeFieldsNoPrefix ''Report

-- Model / Statistic
makeFieldsNoPrefix ''InstanceStatistics

-- Model / Template
makeFieldsNoPrefix ''Template

makeFieldsNoPrefix ''TemplateAllowedPackage

makeFieldsNoPrefix ''TemplateFormat

-- Model / User
makeFieldsNoPrefix ''User

makeFieldsNoPrefix ''UserSubmissionProps

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKey
makeFieldsNoPrefix ''ActionKeyDTO

-- Api / Resource / BookReference
makeFieldsNoPrefix ''BookReferenceDTO

-- Api / Resource / Branch
makeFieldsNoPrefix ''BranchChangeDTO

makeFieldsNoPrefix ''BranchCreateDTO

makeFieldsNoPrefix ''BranchDTO

makeFieldsNoPrefix ''BranchDetailDTO

makeFieldsNoPrefix ''BranchWithEventsDTO

-- Api / Resource / Config
makeFieldsNoPrefix ''AppConfigChangeDTO

makeFieldsNoPrefix ''ClientConfigDTO

makeFieldsNoPrefix ''ClientConfigRegistryDTO

makeFieldsNoPrefix ''ClientConfigQuestionnaireDTO

-- Api / Resource / Document
makeFieldsNoPrefix ''DocumentDTO

makeFieldsNoPrefix ''DocumentCreateDTO

makeFieldsNoPrefix ''DocumentContextDTO

makeFieldsNoPrefix ''DocumentContextConfigDTO

-- Api / Resource / Feedback
makeFieldsNoPrefix ''FeedbackDTO

makeFieldsNoPrefix ''FeedbackCreateDTO

-- Api / Resource / Info
makeFieldsNoPrefix ''InfoDTO

-- Model / Level
makeFieldsNoPrefix ''LevelDTO

-- Api / Resource / Migration / KnowledgeModel
makeFieldsNoPrefix ''KM_MigratorConflictDTO.MigratorConflictDTO

makeFieldsNoPrefix ''KM_createDTO.MigratorStateCreateDTO

makeFieldsNoPrefix ''KM_MigratorStateDetailDTO.MigratorStateDetailDTO

makeFieldsNoPrefix ''KM_MigratorStateDTO.MigratorStateDTO

-- Api / Resource / Migration / Questionnaire
makeFieldsNoPrefix ''QTN_MigratorStateDTO.MigratorStateDTO

makeFieldsNoPrefix ''QTN_createDTO.MigratorStateCreateDTO

makeFieldsNoPrefix ''QTN_changeDTO.MigratorStateChangeDTO

-- Api / Resource / Organization
makeFieldsNoPrefix ''OrganizationSimpleDTO

-- Api / Resource / Package
makeFieldsNoPrefix ''PackageDTO

makeFieldsNoPrefix ''PackageSimpleDTO

makeFieldsNoPrefix ''PackageDetailDTO

-- Api / Resource / PackageBundle
makeFieldsNoPrefix ''PackageBundleDTO

-- Api / Resource / Questionnaire
makeFieldsNoPrefix ''QuestionnaireCreateDTO

makeFieldsNoPrefix ''QuestionnaireDTO

makeFieldsNoPrefix ''QuestionnaireDetailDTO

makeFieldsNoPrefix ''QuestionnaireChangeDTO

makeFieldsNoPrefix ''ReplyDTO

makeFieldsNoPrefix ''ReplyValueDTO

makeFieldsNoPrefix ''IntegrationReplyValueDTO

makeFieldsNoPrefix ''LabelDTO

-- Api / Resource / Registry
makeFieldsNoPrefix ''RegistryConfirmationDTO

makeFieldsNoPrefix ''RegistryCreateDTO

-- Api / Resource / Report
makeFieldsNoPrefix ''IndicationDTO

makeFieldsNoPrefix ''MetricSummaryDTO

makeFieldsNoPrefix ''ChapterReportDTO

makeFieldsNoPrefix ''TotalReportDTO

makeFieldsNoPrefix ''ReportDTO

-- Api / Resource / Submission
makeFieldsNoPrefix ''SubmissionCreateDTO

makeFieldsNoPrefix ''SubmissionDTO

-- Api / Resource / Template
makeFieldsNoPrefix ''TemplateDTO

-- Api / Resource / Token
makeFieldsNoPrefix ''TokenDTO

makeFieldsNoPrefix ''TokenCreateDTO

-- Api / Resource / Typehint
makeFieldsNoPrefix ''TypehintDTO

makeFieldsNoPrefix ''TypehintRequestDTO

-- Api / Resource / User
makeFieldsNoPrefix ''UserChangeDTO

makeFieldsNoPrefix ''UserCreateDTO

makeFieldsNoPrefix ''UserDTO

makeFieldsNoPrefix ''UserPasswordDTO

makeFieldsNoPrefix ''UserProfileDTO

makeFieldsNoPrefix ''UserProfileChangeDTO

makeFieldsNoPrefix ''UserStateDTO

makeFieldsNoPrefix ''UserSubmissionPropsDTO

-- Api / Resource / Version
makeFieldsNoPrefix ''VersionDTO

-- -------------------------------------
-- Integration
-- -------------------------------------
-- Integration / Resource / GitHub
makeFields ''IssueIDTO

-- Integration / Resource / Organization
makeFields ''OrganizationSimpleIDTO

-- Integration / Resource / Package
makeFieldsNoPrefix ''PackageDetailIDTO

makeFieldsNoPrefix ''PackageSimpleIDTO

-- Integration / Resource / Typehint
makeFields ''TypehintIDTO

-- -------------------------------------
-- Registry
-- -------------------------------------
-- Model / ActionKey
makeFieldsNoPrefix ''Registry_ActionKey.ActionKey

-- Api / Resource / Organization
makeFieldsNoPrefix ''OrganizationDTO
