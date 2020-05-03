module Registry.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Shared.Util.Uuid

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO ()
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _serverConfig = baseContext ^. serverConfig
          , _localization = baseContext ^. localization
          , _buildInfoConfig = baseContext ^. buildInfoConfig
          , _pool = baseContext ^. pool
          , _traceUuid = traceUuid
          , _currentOrganization = Nothing
          }
  _ <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  return ()
