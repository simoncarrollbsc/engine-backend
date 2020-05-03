module Wizard.Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv)

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Config.ServerConfig

data AppContext =
  AppContext
    { _serverConfig :: ServerConfig
    , _localization :: M.Map String String
    , _buildInfoConfig :: BuildInfoConfig
    , _pool :: ConnectionPool
    , _msgChannel :: Maybe Channel
    , _httpClientManager :: Manager
    , _registryClient :: ClientEnv
    , _traceUuid :: U.UUID
    , _currentUser :: Maybe UserDTO
    , _shutdownFlag :: MVar ()
    }

newtype AppContextM a =
  AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
