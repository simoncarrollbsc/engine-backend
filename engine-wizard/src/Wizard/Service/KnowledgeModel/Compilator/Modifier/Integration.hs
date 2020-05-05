module Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddIntegrationEvent Integration where
  createEntity e =
    Integration
      { _uuid = e ^. entityUuid
      , _iId = e ^. iId
      , _name = e ^. name
      , _props = e ^. props
      , _logo = e ^. logo
      , _requestMethod = e ^. requestMethod
      , _requestUrl = e ^. requestUrl
      , _requestHeaders = e ^. requestHeaders
      , _requestBody = e ^. requestBody
      , _responseListField = e ^. responseListField
      , _responseIdField = e ^. responseIdField
      , _responseNameField = e ^. responseNameField
      , _itemUrl = e ^. itemUrl
      }

instance EditEntity EditIntegrationEvent Integration where
  editEntity e =
    applyIId .
    applyName .
    applyProps .
    applyLogo .
    applyRequestMethod .
    applyRequestUrl .
    applyRequestHeaders .
    applyRequestBody . applyResponseListField . applyResponseIdField . applyResponseNameField . applyItemUrl
    where
      applyIId integration = applyValue (e ^. iId) integration iId
      applyName integration = applyValue (e ^. name) integration name
      applyProps integration = applyValue (e ^. props) integration props
      applyLogo integration = applyValue (e ^. logo) integration logo
      applyRequestMethod integration = applyValue (e ^. requestMethod) integration requestMethod
      applyRequestUrl integration = applyValue (e ^. requestUrl) integration requestUrl
      applyRequestHeaders integration = applyValue (e ^. requestHeaders) integration requestHeaders
      applyRequestBody integration = applyValue (e ^. requestBody) integration requestBody
      applyResponseListField integration = applyValue (e ^. responseListField) integration responseListField
      applyResponseIdField integration = applyValue (e ^. responseIdField) integration responseIdField
      applyResponseNameField integration = applyValue (e ^. responseNameField) integration responseNameField
      applyItemUrl integration = applyValue (e ^. itemUrl) integration itemUrl
