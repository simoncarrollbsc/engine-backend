module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO where

import qualified Data.UUID as U
import GHC.Generics

data ReplyDTO =
  ReplyDTO
    { _path :: String
    , _value :: ReplyValueDTO
    }
  deriving (Show, Eq, Generic)

data ReplyValueDTO
  = StringReplyDTO
      { _stringValue :: String
      }
  | AnswerReplyDTO
      { _answerValue :: U.UUID
      }
  | ItemListReplyDTO
      { _itemListValue :: Int
      }
  | IntegrationReplyDTO
      { _integrationValue :: IntegrationReplyValueDTO
      }
  deriving (Show, Eq, Generic)

data IntegrationReplyValueDTO
  = PlainValueDTO String
  | IntegrationValueDTO
      { _intId :: String
      , _intValue :: String
      }
  deriving (Show, Eq, Generic)
