module Wizard.Api.Handler.Template.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "templateId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateDetailDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateDetailDTO)
detail_GET mTokenHeader tmlId =
  getServiceTokenOrMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< getTemplateByUuidDto tmlId
