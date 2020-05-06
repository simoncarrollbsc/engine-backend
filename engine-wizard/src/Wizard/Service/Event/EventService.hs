module Wizard.Service.Event.EventService where

import Shared.Model.Event.Event
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService

updateEvents :: String -> [Event] -> AppContextM [Event]
updateEvents branchUuid events = do
  _ <- getBranchById branchUuid
  updateEventsInBranch branchUuid events
  return events

deleteEvents :: String -> AppContextM ()
deleteEvents branchUuid = do
  _ <- getBranchById branchUuid
  deleteEventsAtBranch branchUuid
  return ()
