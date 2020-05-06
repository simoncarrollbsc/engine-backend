module Wizard.Service.Migration.KnowledgeModel.Migrator.CorrectorMethod
  ( runCorrectorMethod
  ) where

import Control.Lens

import LensesConfig
import Shared.Model.Event.Event
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.Migrator.Sanitizator

runCorrectorMethod :: MigratorState -> Event -> IO MigratorState
runCorrectorMethod state event = do
  sanitizedEvent <- sanitize state event
  return $ state & migrationState .~ (ConflictState . CorrectorConflict $ sanitizedEvent)
