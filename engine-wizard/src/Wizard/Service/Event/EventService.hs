module Wizard.Service.Event.EventService where

import Shared.Model.Event.Event
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService

updateEvents :: String -> [Event] -> AppContextM [Event]
updateEvents branchUuid eventsCreateDto = do
  _ <- getBranchById branchUuid
  let events = fromDTOs eventsCreateDto
  updateEventsInBranch branchUuid events
  return . toDTOs $ events

deleteEvents :: String -> AppContextM ()
deleteEvents branchUuid = do
  _ <- getBranchById branchUuid
  deleteEventsAtBranch branchUuid
  return ()
