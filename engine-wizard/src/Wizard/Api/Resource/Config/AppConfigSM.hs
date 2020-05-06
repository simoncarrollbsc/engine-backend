module Wizard.Api.Resource.Config.AppConfigSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.SimpleFeatureSM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.AppConfig

instance ToSchema AppConfig where
  declareNamedSchema = simpleToSchema defaultAppConfig

instance ToSchema AppConfigOrganization where
  declareNamedSchema = simpleToSchema defaultOrganization

instance ToSchema AppConfigAuth where
  declareNamedSchema = simpleToSchema defaultAuth

instance ToSchema AppConfigAuthInternal where
  declareNamedSchema = simpleToSchema defaultAuthInternal

instance ToSchema AppConfigAuthExternal where
  declareNamedSchema = simpleToSchema defaultAuthExternal

instance ToSchema AppConfigAuthExternalService where
  declareNamedSchema = simpleToSchema defaultAuthExternalService

instance ToSchema AppConfigAuthExternalServiceParameter where
  declareNamedSchema = simpleToSchema defaultAuthExternalServiceParameter

instance ToSchema AppConfigAuthExternalServiceStyle where
  declareNamedSchema = simpleToSchema defaultAuthExternalServiceStyle

instance ToSchema AppConfigPrivacyAndSupport where
  declareNamedSchema = simpleToSchema defaultPrivacyAndSupport

instance ToSchema AppConfigDashboard where
  declareNamedSchema = simpleToSchema defaultDashboard

instance ToSchema AppConfigDashboardWidgets where
  declareNamedSchema = simpleToSchema defaultDashboardWidgets

instance ToSchema AppConfigLookAndFeel where
  declareNamedSchema = simpleToSchema defaultLookAndFeel

instance ToSchema AppConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = simpleToSchema defaultLookAndFeelCustomLink

instance ToSchema AppConfigRegistry where
  declareNamedSchema = simpleToSchema defaultRegistry

instance ToSchema AppConfigQuestionnaire where
  declareNamedSchema = simpleToSchema defaultQuestionnaire

instance ToSchema AppConfigQuestionnaireFeedback where
  declareNamedSchema = simpleToSchema defaultFeedback

instance ToSchema AppConfigTemplate where
  declareNamedSchema = simpleToSchema defaultTemplate

instance ToSchema AppConfigSubmission where
  declareNamedSchema = simpleToSchema defaultSubmission

instance ToSchema AppConfigSubmissionService where
  declareNamedSchema = simpleToSchema defaultSubmissionService

instance ToSchema AppConfigSubmissionServiceSupportedFormat where
  declareNamedSchema = simpleToSchema defaultSubmissionServiceSupportedFormat

instance ToSchema AppConfigSubmissionServiceRequest where
  declareNamedSchema = simpleToSchema defaultSubmissionServiceRequest

instance ToSchema AppConfigSubmissionServiceRequestMultipart where
  declareNamedSchema = simpleToSchema defaultSubmissionServiceRequestMultipart
