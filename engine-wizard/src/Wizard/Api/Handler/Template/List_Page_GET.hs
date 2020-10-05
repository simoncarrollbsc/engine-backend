module Wizard.Api.Handler.Template.List_Page_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_Page_GET
   = Header "Authorization" String
     :> "templates"
     :> "page"
     :> QueryParam "organizationId" String
     :> QueryParam "templateId" String
     :> QueryParam "pkgId" String
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page TemplateSimpleDTO))

list_page_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page TemplateSimpleDTO))
list_page_GET mTokenHeader mOrganizationId mTmlId mPkgId mQuery mPage mSize mSort =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<<
    getTemplatesPage mOrganizationId mTmlId mPkgId mQuery (Pageable mPage mSize) (parseSortQuery mSort)