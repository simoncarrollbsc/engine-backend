module Wizard.Service.Migration.KnowledgeModel.Migrator.CleanerMethod where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import LensesExtension
import Wizard.Model.Migration.KnowledgeModel.MigratorState

isCleanerMethod :: MigratorState -> Event -> Bool
isCleanerMethod state event = getKM $ \km -> doIsCleanerMethod km event
  where
    getKM callback =
      case state ^. currentKnowledgeModel of
        Just km -> callback km
        Nothing -> False

doIsCleanerMethod :: KnowledgeModel -> Event -> Bool
doIsCleanerMethod km event@AddKnowledgeModelEvent {} = False
doIsCleanerMethod km event@EditKnowledgeModelEvent {} = False
doIsCleanerMethod km event@AddChapterEvent {} = False
doIsCleanerMethod km event@EditChapterEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. chaptersM)
doIsCleanerMethod km event@DeleteChapterEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. chaptersM)
doIsCleanerMethod km event@AddOptionsQuestionEvent {} =
  isNothing (M.lookup (event ^. parentUuid') (km ^. chaptersM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. questionsM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. answersM))
doIsCleanerMethod km event@AddListQuestionEvent {} =
  isNothing (M.lookup (event ^. parentUuid') (km ^. chaptersM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. questionsM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. answersM))
doIsCleanerMethod km event@AddValueQuestionEvent {} =
  isNothing (M.lookup (event ^. parentUuid') (km ^. chaptersM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. questionsM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. answersM))
doIsCleanerMethod km event@AddIntegrationQuestionEvent {} =
  isNothing (M.lookup (event ^. parentUuid') (km ^. chaptersM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. questionsM)) &&
  isNothing (M.lookup (event ^. parentUuid') (km ^. answersM))
doIsCleanerMethod km event@EditOptionsQuestionEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditListQuestionEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditValueQuestionEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditIntegrationQuestionEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. questionsM)
doIsCleanerMethod km event@DeleteQuestionEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. questionsM)
doIsCleanerMethod km event@AddAnswerEvent {} = isNothing $ M.lookup (event ^. parentUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditAnswerEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. answersM)
doIsCleanerMethod km event@DeleteAnswerEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. answersM)
doIsCleanerMethod km event@AddExpertEvent {} = isNothing $ M.lookup (event ^. parentUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditExpertEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. expertsM)
doIsCleanerMethod km event@DeleteExpertEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. expertsM)
doIsCleanerMethod km event@AddResourcePageReferenceEvent {} = isNothing $ M.lookup (event ^. parentUuid') (km ^. questionsM)
doIsCleanerMethod km event@AddURLReferenceEvent {} = isNothing $ M.lookup (event ^. parentUuid') (km ^. questionsM)
doIsCleanerMethod km event@AddCrossReferenceEvent {} = isNothing $ M.lookup (event ^. parentUuid') (km ^. questionsM)
doIsCleanerMethod km event@EditResourcePageReferenceEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. referencesM)
doIsCleanerMethod km event@EditURLReferenceEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. referencesM)
doIsCleanerMethod km event@EditCrossReferenceEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. referencesM)
doIsCleanerMethod km event@DeleteReferenceEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. referencesM)
doIsCleanerMethod km event@AddTagEvent {} = False
doIsCleanerMethod km event@EditTagEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. tagsM)
doIsCleanerMethod km event@DeleteTagEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. tagsM)
doIsCleanerMethod km event@AddIntegrationEvent {} = False
doIsCleanerMethod km event@EditIntegrationEvent {} = isNothing $ M.lookup (event ^. entityUuid') (km ^. integrationsM)
doIsCleanerMethod km event@DeleteIntegrationEvent {} =
  isNothing $ M.lookup (event ^. entityUuid') (km ^. integrationsM)
doIsCleanerMethod km event@MoveQuestionEvent {} =
  isNothing (M.lookup (event ^. entityUuid') (km ^. questionsM)) ||
  (isNothing (M.lookup (event ^. targetUuid') (km ^. chaptersM)) &&
   isNothing (M.lookup (event ^. targetUuid') (km ^. questionsM)) &&
   isNothing (M.lookup (event ^. targetUuid') (km ^. answersM)))
doIsCleanerMethod km event@MoveAnswerEvent {} =
  isNothing (M.lookup (event ^. entityUuid') (km ^. answersM)) ||
  isNothing (M.lookup (event ^. targetUuid') (km ^. questionsM))
doIsCleanerMethod km event@MoveExpertEvent {} =
  isNothing (M.lookup (event ^. entityUuid') (km ^. expertsM)) ||
  isNothing (M.lookup (event ^. targetUuid') (km ^. questionsM))
doIsCleanerMethod km event@MoveReferenceEvent {} =
  isNothing (M.lookup (event ^. entityUuid') (km ^. referencesM)) ||
  isNothing (M.lookup (event ^. targetUuid') (km ^. questionsM))

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. targetPackageEvents
   in return $ state & targetPackageEvents .~ newTargetPackageEvents
