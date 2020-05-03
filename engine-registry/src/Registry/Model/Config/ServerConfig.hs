module Registry.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment

data ServerConfig =
  ServerConfig
    { _general :: ServerConfigGeneral
    , _database :: ServerConfigDatabase
    , _mail :: ServerConfigMail
    , _analytics :: ServerConfigAnalytics
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _environment :: Environment
    , _clientUrl :: String
    , _serverPort :: Int
    , _templateFolder :: String
    , _remoteLocalizationUrl :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigDatabase =
  ServerConfigDatabase
    { _host :: String
    , _databaseName :: String
    , _port :: Integer
    , _authEnabled :: Bool
    , _username :: Maybe String
    , _password :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigMail =
  ServerConfigMail
    { _enabled :: Bool
    , _name :: Maybe String
    , _email :: Maybe String
    , _host :: Maybe String
    , _port :: Maybe Int
    , _ssl :: Maybe Bool
    , _authEnabled :: Maybe Bool
    , _username :: Maybe String
    , _password :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _enabled :: Bool
    , _email :: Maybe String
    }
  deriving (Generic, Show)
