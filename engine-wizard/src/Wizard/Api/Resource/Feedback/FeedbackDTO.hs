module Wizard.Api.Resource.Feedback.FeedbackDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data FeedbackDTO =
  FeedbackDTO
    { _uuid :: U.UUID
    , _issueId :: Int
    , _issueUrl :: String
    , _questionUuid :: U.UUID
    , _packageId :: String
    , _title :: String
    , _content :: String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq FeedbackDTO where
  a == b =
    _uuid a == _uuid b &&
    _issueId a == _issueId b &&
    _issueUrl a == _issueUrl b &&
    _questionUuid a == _questionUuid b &&
    _packageId a == _packageId b && _title a == _title b && _content a == _content b
