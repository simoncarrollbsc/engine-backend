module Wizard.Service.Document.DocumentMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Model.Document.Document
import Wizard.Model.Template.Template

toDTO :: Document -> Maybe QuestionnaireDTO -> Template -> DocumentDTO
toDTO doc mQtn tml =
  DocumentDTO
    { _uuid = doc ^. uuid
    , _name = doc ^. name
    , _state = doc ^. state
    , _questionnaire = mQtn
    , _template = tml
    , _formatUuid = doc ^. formatUuid
    , _ownerUuid = doc ^. ownerUuid
    , _createdAt = doc ^. createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> DocumentDurability -> Int -> U.UUID -> UTCTime -> Document
fromCreateDTO dto uuid durability repliesHash currentUserUuid now =
  Document
    { _uuid = uuid
    , _name = dto ^. name
    , _state = QueuedDocumentState
    , _durability = durability
    , _questionnaireUuid = dto ^. questionnaireUuid
    , _questionnaireRepliesHash = repliesHash
    , _templateUuid = dto ^. templateUuid
    , _formatUuid = dto ^. formatUuid
    , _metadata = DocumentMetadata {_fileName = Nothing, _contentType = Nothing}
    , _ownerUuid = currentUserUuid
    , _createdAt = now
    }
