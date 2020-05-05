module Wizard.Api.Resource.Config.AppConfigChangeDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig

data AppConfigChangeDTO =
  AppConfigChangeDTO
    { _organization :: AppConfigOrganization
    , _authentication :: AppConfigAuth
    , _privacyAndSupport :: AppConfigPrivacyAndSupport
    , _dashboard :: AppConfigDashboard
    , _lookAndFeel :: AppConfigLookAndFeel
    , _eRegistry :: AppConfigRegistry
    , _questionnaire :: AppConfigQuestionnaire
    , _template :: AppConfigTemplate
    , _submission :: AppConfigSubmission
    }
  deriving (Generic, Show)
