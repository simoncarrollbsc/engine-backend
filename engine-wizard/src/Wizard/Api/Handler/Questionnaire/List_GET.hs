module Wizard.Api.Handler.Questionnaire.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page QuestionnaireDTO))

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page QuestionnaireDTO))
list_GET mTokenHeader mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< getQuestionnairesForCurrentUserPageDto mQuery (Pageable mPage mSize) (parseSortQuery mSort)
