module Wizard.Database.BSON.Questionnaire.QuestionnaireReply where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Shared.Database.BSON.Common ()
import Shared.Database.BSON.KnowledgeModel.KnowledgeModel ()
import Wizard.Model.Questionnaire.QuestionnaireReply

instance FromBSON Reply where
  fromBSON doc = do
    _path <- BSON.lookup "path" doc
    _value <- BSON.lookup "value" doc
    return Reply {..}
  fromBSON' doc = do
    _path <- BSON.lookup "path" doc
    _value <- BSON.lookup "value" doc
    return Reply {..}

instance FromBSON ReplyValue where
  fromBSON doc = do
    rvType <- BSON.lookup "type" doc
    case rvType of
      "StringReply" -> do
        _stringValue <- BSON.lookup "value" doc
        return StringReply {..}
      "AnswerReply" -> do
        _answerValue <- BSON.lookup "value" doc
        return AnswerReply {..}
      "ItemListReply" -> do
        _itemListValue <- BSON.lookup "value" doc
        return ItemListReply {..}
      "IntegrationReply" -> do
        _integrationValue <- BSON.lookup "value" doc
        return IntegrationReply {..}
  fromBSON' doc = do
    rvType <- BSON.lookup "type" doc
    case rvType of
      "StringReply" -> do
        _stringValue <- BSON.lookup "value" doc
        return StringReply {..}
      "AnswerReply" -> do
        _answerValue <- BSON.lookup "value" doc
        return AnswerReply {..}
      "ItemListReply" -> do
        _itemListValue <- BSON.lookup "value" doc
        return ItemListReply {..}
      "IntegrationReply" -> do
        _integrationValue <- BSON.lookup "value" doc
        return IntegrationReply {..}

instance FromBSON IntegrationReplyValue where
  fromBSON doc = do
    intType <- BSON.lookup "type" doc
    case intType of
      "PlainValue" -> do
        value <- BSON.lookup "value" doc
        return $ PlainValue value
      "IntegrationValue" -> do
        _intId <- BSON.lookup "id" doc
        _intValue <- BSON.lookup "value" doc
        return IntegrationValue {..}
  fromBSON' doc = do
    intType <- BSON.lookup "type" doc
    case intType of
      "PlainValue" -> do
        value <- BSON.lookup "value" doc
        return $ PlainValue value
      "IntegrationValue" -> do
        _intId <- BSON.lookup "id" doc
        _intValue <- BSON.lookup "value" doc
        return IntegrationValue {..}

-- --------------------------------------------------------------------
instance ToBSON Reply where
  toBSON reply = ["path" BSON.=: (reply ^. path), "value" BSON.=: (reply ^. value)]
  toBSON' reply = ["path" BSON.=: (reply ^. path), "value" BSON.=: (reply ^. value)]

instance ToBSON ReplyValue where
  toBSON StringReply {..} = ["type" BSON.=: "StringReply", "value" BSON.=: _stringValue]
  toBSON AnswerReply {..} = ["type" BSON.=: "AnswerReply", "value" BSON.=: (_answerValue)]
  toBSON ItemListReply {..} = ["type" BSON.=: "ItemListReply", "value" BSON.=: _itemListValue]
  toBSON IntegrationReply {..} = ["type" BSON.=: "IntegrationReply", "value" BSON.=: _integrationValue]
  toBSON' StringReply {..} = ["type" BSON.=: "StringReply", "value" BSON.=: _stringValue]
  toBSON' AnswerReply {..} = ["type" BSON.=: "AnswerReply", "value" BSON.=: (_answerValue)]
  toBSON' ItemListReply {..} = ["type" BSON.=: "ItemListReply", "value" BSON.=: _itemListValue]
  toBSON' IntegrationReply {..} = ["type" BSON.=: "IntegrationReply", "value" BSON.=: _integrationValue]

instance ToBSON IntegrationReplyValue where
  toBSON (PlainValue value) = ["type" BSON.=: "PlainValue", "value" BSON.=: value]
  toBSON IntegrationValue {..} = ["type" BSON.=: "IntegrationValue", "id" BSON.=: _intId, "value" BSON.=: _intValue]
  toBSON' (PlainValue value) = ["type" BSON.=: "PlainValue", "value" BSON.=: value]
  toBSON' IntegrationValue {..} = ["type" BSON.=: "IntegrationValue", "id" BSON.=: _intId, "value" BSON.=: _intValue]
