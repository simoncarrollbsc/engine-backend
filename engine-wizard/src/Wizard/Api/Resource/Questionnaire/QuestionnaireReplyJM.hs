module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

instance FromJSON ReplyDTO where
  parseJSON = genericParseJSON simpleOptions

instance FromJSON ReplyValueDTO where
  parseJSON (Object o) = do
    rvType <- o .: "type"
    case rvType of
      "StringReply" -> do
        _stringValue <- o .: "value"
        return StringReplyDTO {..}
      "AnswerReply" -> do
        _answerValue <- o .: "value"
        return AnswerReplyDTO {..}
      "ItemListReply" -> do
        _itemListValue <- o .: "value"
        return ItemListReplyDTO {..}
      "IntegrationReply" -> do
        _integrationValue <- o .: "value"
        return IntegrationReplyDTO {..}
      _ -> fail "One of the replies has unsupported reply type"
  parseJSON _ = mzero

instance FromJSON IntegrationReplyValueDTO where
  parseJSON (Object o) = do
    intType <- o .: "type"
    case intType of
      "PlainValue" -> do
        value <- o .: "value"
        return $ PlainValueDTO value
      "IntegrationValue" -> do
        _intId <- o .: "id"
        _intValue <- o .: "value"
        return IntegrationValueDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ReplyDTO where
  toJSON = genericToJSON simpleOptions

instance ToJSON ReplyValueDTO where
  toJSON StringReplyDTO {..} = object ["type" .= "StringReply", "value" .= _stringValue]
  toJSON AnswerReplyDTO {..} = object ["type" .= "AnswerReply", "value" .= _answerValue]
  toJSON ItemListReplyDTO {..} = object ["type" .= "ItemListReply", "value" .= _itemListValue]
  toJSON IntegrationReplyDTO {..} = object ["type" .= "IntegrationReply", "value" .= _integrationValue]

instance ToJSON IntegrationReplyValueDTO where
  toJSON (PlainValueDTO value) = object ["type" .= "PlainValue", "value" .= value]
  toJSON IntegrationValueDTO {..} = object ["type" .= "IntegrationValue", "id" .= _intId, "value" .= _intValue]
