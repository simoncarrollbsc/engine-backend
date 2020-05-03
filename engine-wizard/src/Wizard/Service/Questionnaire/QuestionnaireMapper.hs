module Wizard.Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import Data.Time
import Data.UUID (UUID)

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import qualified Wizard.Service.Package.PackageMapper as PM

toDTO :: Questionnaire -> Package -> QuestionnaireState -> Maybe UserDTO -> QuestionnaireDTO
toDTO questionnaire package state mOwner =
  QuestionnaireDTO
    { _uuid = questionnaire ^. uuid
    , _name = questionnaire ^. name
    , _level = questionnaire ^. level
    , _accessibility = questionnaire ^. accessibility
    , _state = state
    , _package = PM.toSimpleDTO package
    , _owner = mOwner
    , _createdAt = questionnaire ^. createdAt
    , _updatedAt = questionnaire ^. updatedAt
    }

toSimpleDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireState -> Maybe UserDTO -> QuestionnaireDTO
toSimpleDTO questionnaire package state mOwner =
  QuestionnaireDTO
    { _uuid = questionnaire ^. uuid
    , _name = questionnaire ^. name
    , _level = questionnaire ^. level
    , _accessibility = questionnaire ^. accessibility
    , _state = state
    , _package = PM.toSimpleDTO . SPM.toPackage $ package
    , _owner = mOwner
    , _createdAt = questionnaire ^. createdAt
    , _updatedAt = questionnaire ^. updatedAt
    }

toReplyDTO :: Reply -> ReplyDTO
toReplyDTO reply = ReplyDTO {_path = reply ^. path, _value = toReplyValueDTO $ reply ^. value}

toReplyValueDTO :: ReplyValue -> ReplyValueDTO
toReplyValueDTO StringReply {..} = StringReplyDTO {_stringValue = _stringValue}
toReplyValueDTO AnswerReply {..} = AnswerReplyDTO {_answerValue = _answerValue}
toReplyValueDTO ItemListReply {..} = ItemListReplyDTO {_itemListValue = _itemListValue}
toReplyValueDTO IntegrationReply {..} =
  IntegrationReplyDTO {_integrationValue = toIntegrationReplyValueDTO _integrationValue}

toIntegrationReplyValueDTO :: IntegrationReplyValue -> IntegrationReplyValueDTO
toIntegrationReplyValueDTO (PlainValue reply) = PlainValueDTO reply
toIntegrationReplyValueDTO IntegrationValue {..} = IntegrationValueDTO {_intId = _intId, _intValue = _intValue}

toLabelDTO :: Label -> LabelDTO
toLabelDTO label = LabelDTO {_path = label ^. path, _value = label ^. value}

toDetailWithPackageWithEventsDTO ::
     Questionnaire -> PackageWithEvents -> KnowledgeModel -> QuestionnaireState -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO questionnaire package knowledgeModel state =
  QuestionnaireDetailDTO
    { _uuid = questionnaire ^. uuid
    , _name = questionnaire ^. name
    , _level = questionnaire ^. level
    , _accessibility = questionnaire ^. accessibility
    , _state = state
    , _package = PM.toSimpleDTO . SPM.toPackage $ package
    , _selectedTagUuids = questionnaire ^. selectedTagUuids
    , _templateUuid = questionnaire ^. templateUuid
    , _formatUuid = questionnaire ^. formatUuid
    , _knowledgeModel = toKnowledgeModelDTO knowledgeModel
    , _replies = toReplyDTO <$> questionnaire ^. replies
    , _labels = toLabelDTO <$> questionnaire ^. labels
    , _ownerUuid = questionnaire ^. ownerUuid
    , _creatorUuid = questionnaire ^. creatorUuid
    , _createdAt = questionnaire ^. createdAt
    , _updatedAt = questionnaire ^. updatedAt
    }

toDetailWithPackageDTO ::
     Questionnaire -> PackageSimpleDTO -> KnowledgeModel -> QuestionnaireState -> QuestionnaireDetailDTO
toDetailWithPackageDTO questionnaire package knowledgeModel state =
  QuestionnaireDetailDTO
    { _uuid = questionnaire ^. uuid
    , _name = questionnaire ^. name
    , _level = questionnaire ^. level
    , _accessibility = questionnaire ^. accessibility
    , _state = state
    , _package = package
    , _selectedTagUuids = questionnaire ^. selectedTagUuids
    , _templateUuid = questionnaire ^. templateUuid
    , _formatUuid = questionnaire ^. formatUuid
    , _knowledgeModel = toKnowledgeModelDTO knowledgeModel
    , _replies = toReplyDTO <$> questionnaire ^. replies
    , _labels = toLabelDTO <$> questionnaire ^. labels
    , _ownerUuid = questionnaire ^. ownerUuid
    , _creatorUuid = questionnaire ^. creatorUuid
    , _createdAt = questionnaire ^. createdAt
    , _updatedAt = questionnaire ^. updatedAt
    }

fromReplyDTO :: ReplyDTO -> Reply
fromReplyDTO reply = Reply {_path = reply ^. path, _value = fromReplyValueDTO $ reply ^. value}

fromReplyValueDTO :: ReplyValueDTO -> ReplyValue
fromReplyValueDTO StringReplyDTO {..} = StringReply {_stringValue = _stringValue}
fromReplyValueDTO AnswerReplyDTO {..} = AnswerReply {_answerValue = _answerValue}
fromReplyValueDTO ItemListReplyDTO {..} = ItemListReply {_itemListValue = _itemListValue}
fromReplyValueDTO IntegrationReplyDTO {..} =
  IntegrationReply {_integrationValue = fromIntegrationReplyValueDTO _integrationValue}

fromIntegrationReplyValueDTO :: IntegrationReplyValueDTO -> IntegrationReplyValue
fromIntegrationReplyValueDTO (PlainValueDTO reply) = PlainValue reply
fromIntegrationReplyValueDTO IntegrationValueDTO {..} = IntegrationValue {_intId = _intId, _intValue = _intValue}

fromLabelDTO :: LabelDTO -> Label
fromLabelDTO label = Label {_path = label ^. path, _value = label ^. value}

fromChangeDTO ::
     QuestionnaireDetailDTO -> QuestionnaireChangeDTO -> QuestionnaireAccessibility -> UUID -> UTCTime -> Questionnaire
fromChangeDTO qtn dto accessibility currentUserUuid now =
  Questionnaire
    { _uuid = qtn ^. uuid
    , _name = dto ^. name
    , _level = dto ^. level
    , _accessibility = accessibility
    , _packageId = qtn ^. package . pId
    , _selectedTagUuids = qtn ^. selectedTagUuids
    , _templateUuid = dto ^. templateUuid
    , _formatUuid = qtn ^. formatUuid
    , _replies = fromReplyDTO <$> dto ^. replies
    , _labels = fromLabelDTO <$> dto ^. labels
    , _ownerUuid =
        if accessibility /= PublicQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _creatorUuid = qtn ^. creatorUuid
    , _createdAt = qtn ^. createdAt
    , _updatedAt = now
    }

fromQuestionnaireCreateDTO ::
     QuestionnaireCreateDTO -> UUID -> QuestionnaireAccessibility -> UUID -> UTCTime -> UTCTime -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid accessibility currentUserUuid qtnCreatedAt qtnUpdatedAt =
  Questionnaire
    { _uuid = qtnUuid
    , _name = dto ^. name
    , _level = 1
    , _accessibility = accessibility
    , _packageId = dto ^. packageId
    , _selectedTagUuids = dto ^. tagUuids
    , _templateUuid = dto ^. templateUuid
    , _formatUuid = Nothing
    , _replies = []
    , _labels = []
    , _ownerUuid =
        if accessibility /= PublicQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _creatorUuid = Just currentUserUuid
    , _createdAt = qtnCreatedAt
    , _updatedAt = qtnUpdatedAt
    }

fromDetailDTO :: QuestionnaireDetailDTO -> Questionnaire
fromDetailDTO dto =
  Questionnaire
    { _uuid = dto ^. uuid
    , _name = dto ^. name
    , _level = dto ^. level
    , _accessibility = dto ^. accessibility
    , _packageId = dto ^. package . pId
    , _selectedTagUuids = dto ^. selectedTagUuids
    , _templateUuid = dto ^. templateUuid
    , _formatUuid = dto ^. formatUuid
    , _replies = fromReplyDTO <$> dto ^. replies
    , _labels = fromLabelDTO <$> dto ^. labels
    , _ownerUuid = dto ^. ownerUuid
    , _creatorUuid = dto ^. creatorUuid
    , _createdAt = dto ^. createdAt
    , _updatedAt = dto ^. updatedAt
    }
