module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.BSON.Questionnaire.Questionnaire ()
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User

entityName = "questionnaire"

collection = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = createFindEntitiesFn collection

findQuestionnairesForCurrentUserPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page Questionnaire)
findQuestionnairesForCurrentUserPage mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn collection pageable sort =<< sel [regexSel "name" mQuery, qtnOwnerSel]

findQuestionnairesByPackageId :: String -> AppContextM [Questionnaire]
findQuestionnairesByPackageId packageId = createFindEntitiesByFn collection ["packageId" =: packageId]

findQuestionnairesByTemplateId :: String -> AppContextM [Questionnaire]
findQuestionnairesByTemplateId templateId = createFindEntitiesByFn collection ["templateId" =: templateId]

findQuestionnairesOwnedByUser :: String -> AppContextM [Questionnaire]
findQuestionnairesOwnedByUser userUuid = createFindEntitiesByFn collection ["permissions.member.uuid" =: userUuid]

findQuestionnaireById :: String -> AppContextM Questionnaire
findQuestionnaireById = createFindEntityByFn collection entityName "uuid"

findQuestionnaireById' :: String -> AppContextM (Maybe Questionnaire)
findQuestionnaireById' = createFindEntityByFn' collection entityName "uuid"

countQuestionnaires :: AppContextM Int
countQuestionnaires = createCountFn collection

insertQuestionnaire :: Questionnaire -> AppContextM Value
insertQuestionnaire = createInsertFn collection

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = createUpdateByFn collection "uuid" (qtn ^. uuid) qtn

deleteQuestionnaires :: AppContextM ()
deleteQuestionnaires = createDeleteEntitiesFn collection

deleteQuestionnairesFiltered :: [(String, String)] -> AppContextM ()
deleteQuestionnairesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deleteQuestionnaireById :: String -> AppContextM ()
deleteQuestionnaireById = createDeleteEntityByFn collection "uuid"

ensureQuestionnaireTextIndex :: AppContextM Document
ensureQuestionnaireTextIndex = createEnsureTextIndex collection ["name"]

-- ---------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------
qtnOwnerSel = do
  currentUser <- getCurrentUser
  return $
    if currentUser ^. role /= _USER_ROLE_ADMIN
      then let visibleEditOptions = [["visibility" =: show VisibleEditQuestionnaire]]
               visibleViewOptions = [["visibility" =: show VisibleViewQuestionnaire]]
               visiblePrivateUserOptions =
                 [ "visibility" =: show PrivateQuestionnaire
                 , "permissions" =:
                   [ "$elemMatch" =:
                     [ "perms" =: _VIEW_PERM
                     , "member" =: ["type" =: "UserMember", "uuid" =: U.toString (currentUser ^. uuid)]
                     ]
                   ]
                 ]
               visiblePrivateGroupOptions =
                 fmap
                   (\group ->
                      [ "visibility" =: show PrivateQuestionnaire
                      , "permissions" =:
                        [ "$elemMatch" =:
                          ["perms" =: _VIEW_PERM, "member" =: ["type" =: "GroupMember", "uuid" =: group ^. groupId]]
                        ]
                      ])
                   (currentUser ^. groups)
               visiblePrivateOptions = visiblePrivateUserOptions : visiblePrivateGroupOptions
               options = visibleEditOptions ++ visibleViewOptions ++ visiblePrivateOptions
            in ["$or" =: options]
      else []
