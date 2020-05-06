module Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizator where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import LensesExtension
import Shared.Util.String (splitOn)
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Util.Maybe (concatMaybe)

sanitizeReplies :: KnowledgeModel -> [Reply] -> [Reply]
sanitizeReplies km = mapMaybe (sanitizeReply km)

sanitizeReply :: KnowledgeModel -> Reply -> Maybe Reply
sanitizeReply km reply =
  let pathParsed = reverse $ splitOn "." (reply ^. path)
   in case sanitizeQuestion km pathParsed (reply ^. value) of
        Just replyValue -> Just $ reply & value .~ replyValue
        Nothing -> Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeQuestion :: KnowledgeModel -> [String] -> ReplyValue -> Maybe ReplyValue
sanitizeQuestion km (questionUuidS:_) replyValue =
  case concatMaybe $ fmap (\qUuid -> M.lookup qUuid (km ^. questionsM)) . U.fromString $ questionUuidS of
    (Just q@OptionsQuestion {}) -> sanitizeOptionsQuestion km replyValue q
    (Just q@ListQuestion {}) -> sanitizeListQuestion km replyValue q
    (Just q@ValueQuestion {}) -> sanitizeValueQuestion km replyValue q
    (Just q@IntegrationQuestion {}) -> sanitizeIntegrationQuestion km replyValue q
    _ -> Nothing

sanitizeOptionsQuestion :: KnowledgeModel -> ReplyValue -> Question -> Maybe ReplyValue
sanitizeOptionsQuestion km AnswerReply {..} q =
  if _answerReplyValue `elem` (q ^. answerUuids')
    then Just $ AnswerReply {..}
    else Nothing
sanitizeOptionsQuestion _ _ _ = Nothing

sanitizeListQuestion :: KnowledgeModel -> ReplyValue -> Question -> Maybe ReplyValue
sanitizeListQuestion km ItemListReply {..} q = Just $ ItemListReply {..}
sanitizeListQuestion _ _ _ = Nothing

sanitizeValueQuestion :: KnowledgeModel -> ReplyValue -> Question -> Maybe ReplyValue
sanitizeValueQuestion km StringReply {..} q = Just $ StringReply {..}
sanitizeValueQuestion km IntegrationReply {_integrationReplyValue = replyValue} q =
  case replyValue of
    PlainValue value -> Just $ StringReply {_stringReplyValue = value}
    IntegrationValue {..} -> Just $ StringReply {_stringReplyValue = _integrationValueIntValue}
sanitizeValueQuestion _ _ _ = Nothing

sanitizeIntegrationQuestion :: KnowledgeModel -> ReplyValue -> Question -> Maybe ReplyValue
sanitizeIntegrationQuestion km IntegrationReply {..} q = Just $ IntegrationReply {..}
sanitizeIntegrationQuestion km StringReply {..} q =
  Just $ IntegrationReply {_integrationReplyValue = PlainValue _stringReplyValue}
sanitizeIntegrationQuestion _ _ _ = Nothing
