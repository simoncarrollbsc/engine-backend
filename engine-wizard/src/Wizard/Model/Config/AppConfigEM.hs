module Wizard.Model.Config.AppConfigEM where

import Control.Lens ((^.))

import LensesConfig
import Shared.Util.Crypto (encryptAES256)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig

instance SensitiveData AppConfig where
  process key entity =
    entity
      { _authentication = process key (entity ^. authentication)
      , _knowledgeModelRegistry = process key (entity ^. knowledgeModelRegistry)
      , _questionnaire = process key (entity ^. questionnaire)
      }

instance SensitiveData AppConfigOrganization

instance SensitiveData AppConfigAuth where
  process key entity = entity {_external = process key (entity ^. external)}

instance SensitiveData AppConfigAuthInternal

instance SensitiveData AppConfigAuthExternal where
  process key entity = entity {_services = fmap (process key) (entity ^. services)}

instance SensitiveData AppConfigAuthExternalService where
  process key entity =
    entity
      {_clientId = encryptAES256 key (entity ^. clientId), _clientSecret = encryptAES256 key (entity ^. clientSecret)}

instance SensitiveData AppConfigAuthExternalServiceParameter

instance SensitiveData AppConfigAuthExternalServiceStyle

instance SensitiveData AppConfigPrivacyAndSupport

instance SensitiveData AppConfigDashboard

instance SensitiveData AppConfigDashboardWidgets

instance SensitiveData AppConfigLookAndFeel

instance SensitiveData AppConfigLookAndFeelCustomMenuLink

instance SensitiveData AppConfigRegistry where
  process key entity = entity {_token = encryptAES256 key (entity ^. token)}

instance SensitiveData AppConfigQuestionnaire where
  process key entity = entity {_feedback = process key (entity ^. feedback)}

instance SensitiveData AppConfigQuestionnaireFeedback where
  process key entity = entity {_token = encryptAES256 key (entity ^. token)}
