module Wizard.Model.Document.Document where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentState
  = QueuedDocumentState
  | InProgressDocumentState
  | DoneDocumentState
  | ErrorDocumentState
  deriving (Show, Eq, Generic)

data DocumentMetadata =
  DocumentMetadata
    { _fileName :: Maybe String
    , _contentType :: Maybe String
    }
  deriving (Show, Eq, Generic)

data DocumentDurability
  = PersistentDocumentDurability
  | TemporallyDocumentDurability
  deriving (Show, Eq, Generic)

data Document =
  Document
    { _uuid :: U.UUID
    , _name :: String
    , _state :: DocumentState
    , _durability :: DocumentDurability
    , _questionnaireUuid :: U.UUID
    , _questionnaireRepliesHash :: Int
    , _templateUuid :: U.UUID
    , _formatUuid :: U.UUID
    , _metadata :: DocumentMetadata
    , _ownerUuid :: U.UUID
    , _createdAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
