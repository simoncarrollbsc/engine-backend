module Wizard.Database.Migration.Development.Config.Data.AppConfigs where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.User.User

defaultSecret = "01234567890123456789012345678901"

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { _organization = defaultOrganization
    , _authentication = defaultAuth
    , _privacyAndSupport = defaultPrivacyAndSupport
    , _dashboard = defaultDashboard
    , _lookAndFeel = defaultLookAndFeel
    , _eRegistry = defaultRegistry
    , _questionnaire = defaultQuestionnaire
    , _template = defaultTemplate
    , _submission = defaultSubmission
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultAppConfigEncrypted :: AppConfig
defaultAppConfigEncrypted = process defaultSecret defaultAppConfig

defaultOrganization :: AppConfigOrganization
defaultOrganization =
  AppConfigOrganization
    { _name = "Organization Amsterdam"
    , _description = "Some description of Organization Amsterdam"
    , _organizationId = "org.nl.amsterdam"
    , _affiliations = []
    }

defaultAuth :: AppConfigAuth
defaultAuth =
  AppConfigAuth
    {_defaultRole = _USER_ROLE_DATA_STEWARD, _internal = defaultAuthInternal, _external = defaultAuthExternal}

defaultAuthInternal :: AppConfigAuthInternal
defaultAuthInternal = AppConfigAuthInternal {_registration = SimpleFeature True}

defaultAuthExternal :: AppConfigAuthExternal
defaultAuthExternal = AppConfigAuthExternal {_services = [defaultAuthExternalService]}

defaultAuthExternalService :: AppConfigAuthExternalService
defaultAuthExternalService =
  AppConfigAuthExternalService
    { _aId = "google"
    , _name = "Google"
    , _url = "https://accounts.google.com"
    , _clientId = "32559869123-a98908094.apps.googleusercontent.com"
    , _clientSecret = "sad89089023"
    , _parameteres = [defaultAuthExternalServiceParameter]
    , _style = Just defaultAuthExternalServiceStyle
    }

defaultAuthExternalServiceParameter :: AppConfigAuthExternalServiceParameter
defaultAuthExternalServiceParameter = AppConfigAuthExternalServiceParameter {_name = "hd2", _value = "google.com"}

defaultAuthExternalServiceStyle :: AppConfigAuthExternalServiceStyle
defaultAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle {_icon = Just "fa-google", _background = Just "#000", _color = Just "#FFF"}

defaultPrivacyAndSupport :: AppConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    {_privacyUrl = Nothing, _supportEmail = Nothing, _supportRepositoryName = Nothing, _supportRepositoryUrl = Nothing}

defaultDashboard :: AppConfigDashboard
defaultDashboard =
  AppConfigDashboard {_widgets = Just defaultDashboardWidgets, _welcomeWarning = Nothing, _welcomeInfo = Nothing}

defaultDashboardWidgets :: AppConfigDashboardWidgets
defaultDashboardWidgets =
  AppConfigDashboardWidgets {_admin = ["Welcome"], _dataSteward = ["Welcome"], _researcher = ["Welcome"]}

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { _appTitle = Nothing
    , _appTitleShort = Nothing
    , _customMenuLinks = [defaultLookAndFeelCustomLink]
    , _loginInfo = Nothing
    }

defaultLookAndFeelCustomLink :: AppConfigLookAndFeelCustomMenuLink
defaultLookAndFeelCustomLink =
  AppConfigLookAndFeelCustomMenuLink
    {_icon = "faq", _title = "My Link", _url = "http://example.prg", _newWindow = False}

defaultRegistry :: AppConfigRegistry
defaultRegistry = AppConfigRegistry {_enabled = False, _token = "GlobalToken"}

defaultQuestionnaire :: AppConfigQuestionnaire
defaultQuestionnaire =
  AppConfigQuestionnaire
    {_levels = SimpleFeature True, _feedback = defaultFeedback, _questionnaireAccessibility = SimpleFeature True}

defaultFeedback :: AppConfigQuestionnaireFeedback
defaultFeedback =
  AppConfigQuestionnaireFeedback {_enabled = True, _token = "", _owner = "DSWGlobal", _repo = "dsw-test"}

defaultTemplate :: AppConfigTemplate
defaultTemplate = AppConfigTemplate {_recommendedTemplateUuid = Just $ commonWizardTemplate ^. uuid}

defaultSubmission :: AppConfigSubmission
defaultSubmission = AppConfigSubmission {_enabled = True, _services = [defaultSubmissionService]}

defaultSubmissionService :: AppConfigSubmissionService
defaultSubmissionService =
  AppConfigSubmissionService
    { _sId = "mySubmissionServer"
    , _name = "My Submission Server"
    , _description = "Some description"
    , _props = [defaultSubmissionServiceApiTokenProp, defaultSubmissionServiceSecretProp]
    , _supportedFormats = [defaultSubmissionServiceSupportedFormat]
    , _request = defaultSubmissionServiceRequest
    }

defaultSubmissionServiceApiTokenProp :: String
defaultSubmissionServiceApiTokenProp = "API Token"

defaultSubmissionServiceSecretProp :: String
defaultSubmissionServiceSecretProp = "Secret"

defaultSubmissionServiceSupportedFormat :: AppConfigSubmissionServiceSupportedFormat
defaultSubmissionServiceSupportedFormat =
  AppConfigSubmissionServiceSupportedFormat
    {_templateUuid = commonWizardTemplate ^. uuid, _formatUuid = templateFormatJson ^. uuid}

defaultSubmissionServiceRequest :: AppConfigSubmissionServiceRequest
defaultSubmissionServiceRequest =
  AppConfigSubmissionServiceRequest
    { _method = "GET"
    , _url = "https://mockserver.ds-wizard.org/submission.json"
    , _headers = M.fromList [("Api-Key", "${API Token}")]
    , _multipart = defaultSubmissionServiceRequestMultipart
    }

defaultSubmissionServiceRequestMultipart :: AppConfigSubmissionServiceRequestMultipart
defaultSubmissionServiceRequestMultipart =
  AppConfigSubmissionServiceRequestMultipart {_enabled = False, _fileName = "file"}

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedAppConfig :: AppConfig
editedAppConfig = defaultAppConfig {_questionnaire = editedQuestionnaire}

editedQuestionnaire :: AppConfigQuestionnaire
editedQuestionnaire = defaultQuestionnaire {_levels = SimpleFeature False}
