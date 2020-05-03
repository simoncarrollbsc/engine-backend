module Wizard.Model.Feedback.Feedback where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Feedback =
  Feedback
    { _uuid :: U.UUID
    , _issueId :: Int
    , _questionUuid :: U.UUID
    , _packageId :: String
    , _title :: String
    , _content :: String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq Feedback where
  a == b =
    _uuid a == _uuid b &&
    _issueId a == _issueId b &&
    _questionUuid a == _questionUuid b &&
    _packageId a == _packageId b && _title a == _title b && _content a == _content b
