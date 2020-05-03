module Wizard.Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature

toClientConfigDTO :: ServerConfig -> AppConfig -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig =
  ClientConfigDTO
    { _organization = appConfig ^. organization
    , _authentication = toClientAuthDTO $ appConfig ^. authentication
    , _privacyAndSupport = appConfig ^. privacyAndSupport
    , _dashboard = appConfig ^. dashboard
    , _lookAndFeel = appConfig ^. lookAndFeel
    , _knowledgeModelRegistry =
        toClientConfigRegistryDTO (serverConfig ^. registry) (appConfig ^. knowledgeModelRegistry)
    , _questionnaire = toClientConfigQuestionnaireDTO $ appConfig ^. questionnaire
    , _template = appConfig ^. template
    , _submission = SimpleFeature $ appConfig ^. submission . enabled
    }

toClientAuthDTO :: AppConfigAuth -> ClientConfigAuthDTO
toClientAuthDTO config =
  ClientConfigAuthDTO
    { _defaultRole = config ^. defaultRole
    , _internal = config ^. internal
    , _external = toClientAuthExternalDTO $ config ^. external
    }

toClientAuthExternalDTO :: AppConfigAuthExternal -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO config =
  ClientConfigAuthExternalDTO {_services = toClientAuthExternalServiceDTO <$> config ^. services}

toClientAuthExternalServiceDTO :: AppConfigAuthExternalService -> ClientConfigAuthExternalServiceDTO
toClientAuthExternalServiceDTO config =
  ClientConfigAuthExternalServiceDTO
    {_aId = config ^. aId, _name = config ^. name, _url = config ^. url, _style = config ^. style}

toClientConfigRegistryDTO :: ServerConfigRegistry -> AppConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO serverConfig appConfig =
  ClientConfigRegistryDTO {_enabled = appConfig ^. enabled, _url = serverConfig ^. clientUrl}

toClientConfigQuestionnaireDTO :: AppConfigQuestionnaire -> ClientConfigQuestionnaireDTO
toClientConfigQuestionnaireDTO appConfig =
  ClientConfigQuestionnaireDTO
    { _questionnaireAccessibility = appConfig ^. questionnaireAccessibility
    , _levels = appConfig ^. levels
    , _feedback = SimpleFeature $ appConfig ^. feedback . enabled
    }
