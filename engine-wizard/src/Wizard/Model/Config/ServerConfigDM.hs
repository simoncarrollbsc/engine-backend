module Wizard.Model.Config.ServerConfigDM where

import Shared.Model.Config.Environment
import Wizard.Model.Config.ServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { _general = defaultGeneral
    , _database = defaultDatabase
    , _messaging = defaultMessaging
    , _jwt = defaultJwt
    , _roles = defaultRoles
    , _mail = defaultMail
    , _registry = defaultRegistry
    , _analytics = defaultAnalytics
    , _feedback = defaultFeedback
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { _environment = Production
    , _clientUrl = ""
    , _serverPort = 3000
    , _serviceToken = ""
    , _secret = ""
    , _integrationConfig = "engine-wizard/config/integration.yml"
    , _templateFolder = "engine-wizard/templates"
    , _remoteLocalizationUrl = Nothing
    , _debugLogHttpClient = True
    }

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { _host = "mongo"
    , _databaseName = "wizard-server"
    , _port = 27017
    , _authEnabled = False
    , _username = ""
    , _password = ""
    }

defaultMessaging :: ServerConfigMessaging
defaultMessaging =
  ServerConfigMessaging
    {_enabled = True, _host = "rabbitmq", _port = 5672, _username = "guest", _password = "guest", _vhost = "/"}

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {_version = 1, _expiration = 14}

defaultRoles :: ServerConfigRoles
defaultRoles =
  ServerConfigRoles
    { _admin =
        [ "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        ]
    , _dataSteward =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "SUBM_PERM"
        ]
    , _researcher = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    }

defaultMail :: ServerConfigMail
defaultMail =
  ServerConfigMail
    { _enabled = True
    , _name = "DS Wizard"
    , _email = ""
    , _host = ""
    , _port = 465
    , _ssl = False
    , _authEnabled = False
    , _username = ""
    , _password = ""
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry {_url = "https://api.registry.ds-wizard.org", _clientUrl = "https://registry.ds-wizard.org"}

defaultAnalytics :: ServerConfigAnalytics
defaultAnalytics = ServerConfigAnalytics {_enabled = False, _email = ""}

defaultFeedback :: ServerConfigFeedback
defaultFeedback = ServerConfigFeedback {_apiUrl = "https://api.github.com", _webUrl = "https://github.com"}
