module Registry.Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)

import Registry.Model.Config.ServerConfig
import Registry.Model.Organization.Organization
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error

data AppContext =
  AppContext
    { _serverConfig :: ServerConfig
    , _localization :: M.Map String String
    , _buildInfoConfig :: BuildInfoConfig
    , _pool :: ConnectionPool
    , _traceUuid :: U.UUID
    , _currentOrganization :: Maybe Organization
    }

newtype AppContextM a =
  AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT AppError IO)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError AppError, MonadLogger)
