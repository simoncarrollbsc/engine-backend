module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.User.User

data ClientConfigDTO =
  ClientConfigDTO
    { _organization :: AppConfigOrganization
    , _authentication :: ClientConfigAuthDTO
    , _privacyAndSupport :: AppConfigPrivacyAndSupport
    , _dashboard :: AppConfigDashboard
    , _lookAndFeel :: AppConfigLookAndFeel
    , _eRegistry :: ClientConfigRegistryDTO
    , _questionnaire :: ClientConfigQuestionnaireDTO
    , _template :: AppConfigTemplate
    , _submission :: SimpleFeature
    }
  deriving (Show, Eq, Generic)

data ClientConfigAuthDTO =
  ClientConfigAuthDTO
    { _defaultRole :: Role
    , _internal :: AppConfigAuthInternal
    , _external :: ClientConfigAuthExternalDTO
    }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalDTO =
  ClientConfigAuthExternalDTO
    { _services :: [ClientConfigAuthExternalServiceDTO]
    }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalServiceDTO =
  ClientConfigAuthExternalServiceDTO
    { _aId :: String
    , _name :: String
    , _url :: String
    , _style :: Maybe AppConfigAuthExternalServiceStyle
    }
  deriving (Generic, Eq, Show)

data ClientConfigRegistryDTO =
  ClientConfigRegistryDTO
    { _enabled :: Bool
    , _url :: String
    }
  deriving (Show, Eq, Generic)

data ClientConfigQuestionnaireDTO =
  ClientConfigQuestionnaireDTO
    { _questionnaireAccessibility :: SimpleFeature
    , _levels :: SimpleFeature
    , _feedback :: SimpleFeature
    }
  deriving (Generic, Eq, Show)
