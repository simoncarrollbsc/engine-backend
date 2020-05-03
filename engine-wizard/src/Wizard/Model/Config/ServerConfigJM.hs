module Wizard.Model.Config.ServerConfigJM where

import Control.Lens ((^.))
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

import LensesConfig
import Shared.Model.Config.EnvironmentJM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigDM
import Wizard.Model.User.User

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    _general <- o .: "general"
    _database <- o .:? "database" .!= defaultDatabase
    _messaging <- o .:? "messaging" .!= defaultMessaging
    _jwt <- o .:? "jwt" .!= defaultJwt
    _roles <- o .:? "roles" .!= defaultRoles
    _mail <- o .:? "mail" .!= defaultMail
    _registry <- o .:? "registry" .!= defaultRegistry
    _analytics <- o .:? "analytics" .!= defaultAnalytics
    _feedback <- o .:? "feedback" .!= defaultFeedback
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    _environment <- o .:? "environment" .!= (defaultGeneral ^. environment)
    _clientUrl <- o .: "clientUrl"
    _serverPort <- o .:? "serverPort" .!= (defaultGeneral ^. serverPort)
    _serviceToken <- o .: "serviceToken"
    _secret <- o .: "secret"
    _integrationConfig <- o .:? "integrationConfig" .!= (defaultGeneral ^. integrationConfig)
    _templateFolder <- o .:? "templateFolder" .!= (defaultGeneral ^. templateFolder)
    _remoteLocalizationUrl <- o .:? "remoteLocalizationUrl" .!= (defaultGeneral ^. remoteLocalizationUrl)
    _debugLogHttpClient <- o .:? "debugLogHttpClient" .!= (defaultGeneral ^. debugLogHttpClient)
    return ServerConfigGeneral {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigDatabase where
  parseJSON (Object o) = do
    _host <- o .:? "host" .!= (defaultDatabase ^. host)
    _databaseName <- o .:? "databaseName" .!= (defaultDatabase ^. databaseName)
    _port <- o .:? "port" .!= (defaultDatabase ^. port)
    _authEnabled <- o .:? "authEnabled" .!= (defaultDatabase ^. authEnabled)
    _username <- o .:? "username" .!= (defaultDatabase ^. username)
    _password <- o .:? "password" .!= (defaultDatabase ^. password)
    return ServerConfigDatabase {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigMessaging where
  parseJSON (Object o) = do
    _enabled <- o .:? "enabled" .!= (defaultMessaging ^. enabled)
    _host <- o .:? "host" .!= (defaultMessaging ^. host)
    _port <- o .:? "port" .!= (defaultMessaging ^. port)
    _username <- o .:? "username" .!= (defaultMessaging ^. username)
    _password <- o .:? "password" .!= (defaultMessaging ^. password)
    _vhost <- o .:? "vhost" .!= (defaultMessaging ^. vhost)
    return ServerConfigMessaging {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigJwt where
  parseJSON (Object o) = do
    _version <- o .:? "version" .!= (defaultJwt ^. version)
    _expiration <- o .:? "expiration" .!= (defaultJwt ^. expiration)
    return ServerConfigJwt {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRoles where
  parseJSON (Object o) = do
    _admin <- o .:? T.pack _USER_ROLE_ADMIN .!= (defaultRoles ^. admin)
    _dataSteward <- o .:? T.pack _USER_ROLE_DATA_STEWARD .!= (defaultRoles ^. dataSteward)
    _researcher <- o .:? T.pack _USER_ROLE_RESEARCHER .!= (defaultRoles ^. researcher)
    return ServerConfigRoles {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigMail where
  parseJSON (Object o) = do
    _enabled <- o .:? "enabled" .!= (defaultMail ^. enabled)
    _name <- o .:? "name" .!= (defaultMail ^. name)
    _email <- o .: "email" .!= (defaultMail ^. email)
    _ssl <- o .:? "ssl" .!= (defaultMail ^. ssl)
    _host <- o .: "host" .!= (defaultMail ^. host)
    _port <-
      o .:? "port" .!=
      (if _ssl
         then 465
         else 25)
    _authEnabled <- o .:? "authEnabled" .!= (defaultMail ^. authEnabled)
    _username <- o .:? "username" .!= (defaultMail ^. username)
    _password <- o .:? "password" .!= (defaultMail ^. password)
    return ServerConfigMail {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRegistry where
  parseJSON (Object o) = do
    _url <- o .:? "url" .!= (defaultRegistry ^. url)
    _clientUrl <- o .:? "clientUrl" .!= (defaultRegistry ^. clientUrl)
    return ServerConfigRegistry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAnalytics where
  parseJSON (Object o) = do
    _enabled <- o .:? "enabled" .!= (defaultAnalytics ^. enabled)
    _email <- o .:? "email" .!= (defaultAnalytics ^. email)
    return ServerConfigAnalytics {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigFeedback where
  parseJSON (Object o) = do
    _apiUrl <- o .:? "apiUrl" .!= (defaultFeedback ^. apiUrl)
    _webUrl <- o .:? "webUrl" .!= (defaultFeedback ^. webUrl)
    return ServerConfigFeedback {..}
  parseJSON _ = mzero
