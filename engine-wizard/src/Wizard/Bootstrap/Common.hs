module Wizard.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO ()
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _serverConfig = baseContext ^. serverConfig
          , _localization = baseContext ^. localization
          , _buildInfoConfig = baseContext ^. buildInfoConfig
          , _pool = baseContext ^. pool
          , _msgChannel = baseContext ^. msgChannel
          , _httpClientManager = baseContext ^. httpClientManager
          , _registryClient = baseContext ^. registryClient
          , _traceUuid = traceUuid
          , _currentUser = Nothing
          , _shutdownFlag = baseContext ^. shutdownFlag
          }
  _ <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  return ()
