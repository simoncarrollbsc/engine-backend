module Wizard.Model.Config.AppConfig where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Config.SimpleFeature
import Wizard.Model.User.User

data AppConfig =
  AppConfig
    { _organization :: AppConfigOrganization
    , _authentication :: AppConfigAuth
    , _privacyAndSupport :: AppConfigPrivacyAndSupport
    , _dashboard :: AppConfigDashboard
    , _lookAndFeel :: AppConfigLookAndFeel
    , _knowledgeModelRegistry :: AppConfigRegistry
    , _questionnaire :: AppConfigQuestionnaire
    , _template :: AppConfigTemplate
    , _submission :: AppConfigSubmission
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq AppConfig where
  a == b =
    _organization a == _organization b &&
    _authentication a == _authentication b &&
    _privacyAndSupport a == _privacyAndSupport b &&
    _dashboard a == _dashboard b &&
    _lookAndFeel a == _lookAndFeel b &&
    _knowledgeModelRegistry a == _knowledgeModelRegistry b &&
    _questionnaire a == _questionnaire b && _submission a == _submission b

data AppConfigOrganization =
  AppConfigOrganization
    { _name :: String
    , _description :: String
    , _organizationId :: String
    , _affiliations :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigAuth =
  AppConfigAuth
    { _defaultRole :: Role
    , _internal :: AppConfigAuthInternal
    , _external :: AppConfigAuthExternal
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthInternal =
  AppConfigAuthInternal
    { _registration :: SimpleFeature
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternal =
  AppConfigAuthExternal
    { _services :: [AppConfigAuthExternalService]
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalService =
  AppConfigAuthExternalService
    { _aId :: String
    , _name :: String
    , _url :: String
    , _clientId :: String
    , _clientSecret :: String
    , _parameteres :: [AppConfigAuthExternalServiceParameter]
    , _style :: Maybe AppConfigAuthExternalServiceStyle
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceParameter =
  AppConfigAuthExternalServiceParameter
    { _name :: String
    , _value :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle
    { _icon :: Maybe String
    , _background :: Maybe String
    , _color :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    { _privacyUrl :: Maybe String
    , _supportEmail :: Maybe String
    , _supportRepositoryName :: Maybe String
    , _supportRepositoryUrl :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigDashboard =
  AppConfigDashboard
    { _widgets :: Maybe AppConfigDashboardWidgets
    , _welcomeWarning :: Maybe String
    , _welcomeInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigDashboardWidgets =
  AppConfigDashboardWidgets
    { _admin :: [String]
    , _dataSteward :: [String]
    , _researcher :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeel =
  AppConfigLookAndFeel
    { _appTitle :: Maybe String
    , _appTitleShort :: Maybe String
    , _customMenuLinks :: [AppConfigLookAndFeelCustomMenuLink]
    , _loginInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeelCustomMenuLink =
  AppConfigLookAndFeelCustomMenuLink
    { _icon :: String
    , _title :: String
    , _url :: String
    , _newWindow :: Bool
    }
  deriving (Show, Eq, Generic)

data AppConfigRegistry =
  AppConfigRegistry
    { _enabled :: Bool
    , _token :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaire =
  AppConfigQuestionnaire
    { _questionnaireAccessibility :: SimpleFeature
    , _levels :: SimpleFeature
    , _feedback :: AppConfigQuestionnaireFeedback
    }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaireFeedback =
  AppConfigQuestionnaireFeedback
    { _enabled :: Bool
    , _token :: String
    , _owner :: String
    , _repo :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigTemplate =
  AppConfigTemplate
    { _recommendedTemplateUuid :: Maybe U.UUID
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmission =
  AppConfigSubmission
    { _enabled :: Bool
    , _services :: [AppConfigSubmissionService]
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionService =
  AppConfigSubmissionService
    { _sId :: String
    , _name :: String
    , _description :: String
    , _props :: [String]
    , _supportedFormats :: [AppConfigSubmissionServiceSupportedFormat]
    , _request :: AppConfigSubmissionServiceRequest
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceSupportedFormat =
  AppConfigSubmissionServiceSupportedFormat
    { _templateUuid :: U.UUID
    , _formatUuid :: U.UUID
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequest =
  AppConfigSubmissionServiceRequest
    { _method :: String
    , _url :: String
    , _headers :: M.Map String String
    , _multipart :: AppConfigSubmissionServiceRequestMultipart
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequestMultipart =
  AppConfigSubmissionServiceRequestMultipart
    { _enabled :: Bool
    , _fileName :: String
    }
  deriving (Generic, Eq, Show)
