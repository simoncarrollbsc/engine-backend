module Wizard.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Util.Uuid
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext

getActionKeyByHash :: String -> AppContextM ActionKey
getActionKeyByHash = findActionKeyByHash

createActionKey :: U.UUID -> ActionKeyType -> AppContextM ActionKey
createActionKey userId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let actionKey =
        ActionKey {_uuid = uuid, _userId = userId, _aType = actionType, _hash = U.toString hash, _createdAt = now}
  insertActionKey actionKey
  return actionKey

deleteActionKey :: String -> AppContextM ()
deleteActionKey hash = do
  actionKey <- getActionKeyByHash hash
  deleteActionKeyByHash hash
  return ()
