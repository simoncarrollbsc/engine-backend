module Wizard.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment
import Wizard.Model.User.User

data ServerConfig =
  ServerConfig
    { _general :: ServerConfigGeneral
    , _database :: ServerConfigDatabase
    , _messaging :: ServerConfigMessaging
    , _jwt :: ServerConfigJwt
    , _roles :: ServerConfigRoles
    , _mail :: ServerConfigMail
    , _registry :: ServerConfigRegistry
    , _analytics :: ServerConfigAnalytics
    , _feedback :: ServerConfigFeedback
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _environment :: Environment
    , _clientUrl :: String
    , _serverPort :: Int
    , _serviceToken :: String
    , _secret :: String
    , _integrationConfig :: String
    , _templateFolder :: String
    , _remoteLocalizationUrl :: Maybe String
    , _debugLogHttpClient :: Bool
    }
  deriving (Generic, Show)

data ServerConfigDatabase =
  ServerConfigDatabase
    { _host :: String
    , _databaseName :: String
    , _port :: Integer
    , _authEnabled :: Bool
    , _username :: String
    , _password :: String
    }
  deriving (Generic, Show)

data ServerConfigMessaging =
  ServerConfigMessaging
    { _enabled :: Bool
    , _host :: String
    , _port :: Integer
    , _username :: String
    , _password :: String
    , _vhost :: String
    }
  deriving (Generic, Show)

data ServerConfigJwt =
  ServerConfigJwt
    { _version :: Integer
    , _expiration :: Integer
    }
  deriving (Generic, Show)

data ServerConfigRoles =
  ServerConfigRoles
    { _admin :: [Permission]
    , _dataSteward :: [Permission]
    , _researcher :: [Permission]
    }
  deriving (Generic, Show)

data ServerConfigMail =
  ServerConfigMail
    { _enabled :: Bool
    , _name :: String
    , _email :: String
    , _host :: String
    , _port :: Int
    , _ssl :: Bool
    , _authEnabled :: Bool
    , _username :: String
    , _password :: String
    }
  deriving (Generic, Show)

data ServerConfigRegistry =
  ServerConfigRegistry
    { _url :: String
    , _clientUrl :: String
    }
  deriving (Generic, Show)

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _enabled :: Bool
    , _email :: String
    }
  deriving (Generic, Show)

data ServerConfigFeedback =
  ServerConfigFeedback
    { _apiUrl :: String
    , _webUrl :: String
    }
  deriving (Generic, Show)
