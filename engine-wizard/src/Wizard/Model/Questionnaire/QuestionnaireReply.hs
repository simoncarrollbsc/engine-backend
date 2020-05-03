module Wizard.Model.Questionnaire.QuestionnaireReply where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

data Reply =
  Reply
    { _path :: String
    , _value :: ReplyValue
    }
  deriving (Show, Eq, Generic)

instance Hashable Reply

data ReplyValue
  = StringReply
      { _stringValue :: String
      }
  | AnswerReply
      { _answerValue :: U.UUID
      }
  | ItemListReply
      { _itemListValue :: Int
      }
  | IntegrationReply
      { _integrationValue :: IntegrationReplyValue
      }
  deriving (Show, Eq, Generic)

instance Hashable ReplyValue

data IntegrationReplyValue
  = PlainValue String
  | IntegrationValue
      { _intId :: String
      , _intValue :: String
      }
  deriving (Show, Eq, Generic)

instance Hashable IntegrationReplyValue
