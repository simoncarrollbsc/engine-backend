module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Model.Config.AppConfig

toChangeDTO :: AppConfig -> AppConfigChangeDTO
toChangeDTO config =
  AppConfigChangeDTO
    { _organization = config ^. organization
    , _authentication = config ^. authentication
    , _privacyAndSupport = config ^. privacyAndSupport
    , _dashboard = config ^. dashboard
    , _lookAndFeel = config ^. lookAndFeel
    , _eRegistry = config ^. knowledgeModelRegistry
    , _questionnaire = config ^. questionnaire
    , _template = config ^. template
    , _submission = config ^. submission
    }

fromChangeDTO :: AppConfigChangeDTO -> AppConfig -> UTCTime -> AppConfig
fromChangeDTO dto oldConfig now =
  AppConfig
    { _organization = dto ^. organization
    , _authentication = dto ^. authentication
    , _privacyAndSupport = dto ^. privacyAndSupport
    , _dashboard = dto ^. dashboard
    , _lookAndFeel = dto ^. lookAndFeel
    , _eRegistry = dto ^. knowledgeModelRegistry
    , _questionnaire = dto ^. questionnaire
    , _template = dto ^. template
    , _submission = dto ^. submission
    , _createdAt = oldConfig ^. createdAt
    , _updatedAt = now
    }
