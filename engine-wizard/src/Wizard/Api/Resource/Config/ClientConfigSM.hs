module Wizard.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import qualified Wizard.Database.Migration.Development.Config.Data.AppConfigs as A
import qualified Wizard.Model.Config.ServerConfigDM as S
import Wizard.Service.Config.ClientConfigMapper

instance ToSchema ClientConfigDTO where
  declareNamedSchema = simpleToSchema "_clientConfigDTO" (toClientConfigDTO S.defaultConfig A.defaultAppConfig)

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = simpleToSchema "_clientConfigAuthDTO" (toClientAuthDTO A.defaultAuth)

instance ToSchema ClientConfigAuthExternalDTO where
  declareNamedSchema = simpleToSchema "_clientConfigAuthExternalDTO" (toClientAuthExternalDTO A.defaultAuthExternal)

instance ToSchema ClientConfigAuthExternalServiceDTO where
  declareNamedSchema =
    simpleToSchema "_clientConfigAuthExternalServiceDTO" (toClientAuthExternalServiceDTO A.defaultAuthExternalService)

instance ToSchema ClientConfigRegistryDTO where
  declareNamedSchema =
    simpleToSchema "_clientConfigRegistryDTO" (toClientConfigRegistryDTO S.defaultRegistry A.defaultRegistry)

instance ToSchema ClientConfigQuestionnaireDTO where
  declareNamedSchema =
    simpleToSchema "_clientConfigQuestionnaireDTO" (toClientConfigQuestionnaireDTO A.defaultQuestionnaire)
