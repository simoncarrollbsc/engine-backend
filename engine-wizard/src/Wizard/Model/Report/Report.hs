module Wizard.Model.Report.Report where

import Data.Time
import qualified Data.UUID as U

data Report =
  Report
    { _uuid :: U.UUID
    , _totalReport :: TotalReport
    , _reports :: [ChapterReport]
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show)

instance Eq Report where
  a == b = _uuid a == _uuid b && _totalReport a == _totalReport b && _reports a == _reports b

data TotalReport =
  TotalReport
    { _indications :: [Indication]
    , _metrics :: [MetricSummary]
    }
  deriving (Show, Eq)

data ChapterReport =
  ChapterReport
    { _uuid :: U.UUID
    , _indications :: [Indication]
    , _metrics :: [MetricSummary]
    }
  deriving (Show, Eq)

data Indication
  = AnsweredIndication
      { _answeredQuestions :: Int
      , _unansweredQuestions :: Int
      }
  | LevelsAnsweredIndication
      { _answeredQuestions :: Int
      , _unansweredQuestions :: Int
      }
  deriving (Show, Eq)

data MetricSummary =
  MetricSummary
    { _uuid :: U.UUID
    , _measure :: Maybe Double
    }
  deriving (Show, Eq)
