module Wizard.Api.Resource.Report.ReportDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ReportDTO =
  ReportDTO
    { _uuid :: U.UUID
    , _totalReport :: TotalReportDTO
    , _reports :: [ChapterReportDTO]
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ReportDTO where
  a == b = _uuid a == _uuid b && _totalReport a == _totalReport b && _reports a == _reports b

data TotalReportDTO =
  TotalReportDTO
    { _indications :: [IndicationDTO]
    , _metrics :: [MetricSummaryDTO]
    }
  deriving (Show, Eq, Generic)

data ChapterReportDTO =
  ChapterReportDTO
    { _uuid :: U.UUID
    , _indications :: [IndicationDTO]
    , _metrics :: [MetricSummaryDTO]
    }
  deriving (Show, Eq, Generic)

data IndicationDTO
  = AnsweredIndicationDTO
      { _answeredQuestions :: Int
      , _unansweredQuestions :: Int
      }
  | LevelsAnsweredIndicationDTO
      { _answeredQuestions :: Int
      , _unansweredQuestions :: Int
      }
  deriving (Show, Eq, Generic)

data MetricSummaryDTO =
  MetricSummaryDTO
    { _uuid :: U.UUID
    , _measure :: Double
    }
  deriving (Show, Eq, Generic)
