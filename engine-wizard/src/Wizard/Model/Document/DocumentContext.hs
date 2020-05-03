module Wizard.Model.Document.DocumentContext where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import qualified Shared.Model.KnowledgeModel.KnowledgeModel as KnowledgeModel
import qualified Shared.Model.Package.Package as Package
import qualified Wizard.Model.Config.AppConfig as AppConfig
import qualified Wizard.Model.Level.Level as Level
import qualified Wizard.Model.Questionnaire.QuestionnaireReply as QuestionnaireReply
import qualified Wizard.Model.Report.Report as Report
import qualified Wizard.Model.User.User as User

data DocumentFormat
  = JSON
  | HTML
  | PDF
  | LaTeX
  | Docx
  | ODT
  | Markdown
  | RTF
  | RST
  | AsciiDoc
  | DokuWiki
  | MediaWiki
  | EPUB2
  | EPUB3
  deriving (Show, Eq, Enum, Bounded, Generic)

data DocumentContext =
  DocumentContext
    { _uuid :: U.UUID
    , _config :: DocumentContextConfig
    , _questionnaireUuid :: String
    , _questionnaireName :: String
    , _questionnaireReplies :: [QuestionnaireReply.Reply]
    , _level :: Int
    , _knowledgeModel :: KnowledgeModel.KnowledgeModel
    , _metrics :: [KnowledgeModel.Metric]
    , _levels :: [Level.Level]
    , _report :: Report.Report
    , _package :: Package.Package
    , _organization :: AppConfig.AppConfigOrganization
    , _createdBy :: Maybe User.User
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DocumentContext where
  a == b =
    _uuid a == _uuid b &&
    _config a == _config b &&
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _level a == _level b &&
    _knowledgeModel a == _knowledgeModel b &&
    _metrics a == _metrics b &&
    _levels a == _levels b &&
    _report a == _report b &&
    _package a == _package b && _organization a == _organization b && _createdBy a == _createdBy b

data DocumentContextConfig =
  DocumentContextConfig
    { _levelsEnabled :: Bool
    }
  deriving (Show, Eq, Generic)
