module Wizard.Api.Resource.Config.AppConfigJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Model.Config.AppConfig

instance FromJSON AppConfig where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfig where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigOrganization where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigOrganization where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuth where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuth where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuthInternal where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuthInternal where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuthExternal where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuthExternal where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuthExternalService where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuthExternalService where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuthExternalServiceParameter where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuthExternalServiceParameter where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigAuthExternalServiceStyle where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigAuthExternalServiceStyle where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigPrivacyAndSupport where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigPrivacyAndSupport where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigDashboard where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigDashboard where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigDashboardWidgets where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigDashboardWidgets where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigLookAndFeel where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigLookAndFeel where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigLookAndFeelCustomMenuLink where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigLookAndFeelCustomMenuLink where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigRegistry where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigRegistry where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigQuestionnaire where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigQuestionnaire where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigQuestionnaireFeedback where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigQuestionnaireFeedback where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigTemplate where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigTemplate where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigSubmission where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigSubmission where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigSubmissionService where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigSubmissionService where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigSubmissionServiceSupportedFormat where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigSubmissionServiceSupportedFormat where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigSubmissionServiceRequest where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigSubmissionServiceRequest where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON AppConfigSubmissionServiceRequestMultipart where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON AppConfigSubmissionServiceRequestMultipart where
  toJSON = genericToJSON simpleOptions'''
