module Wizard.Database.Migration.Development.Report.Data.Reports where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Wizard.Model.Report.Report

report1 :: Report
report1 =
  Report
    { _uuid = fromJust (U.fromString "921bcb7e-e15f-49e4-b176-dbbe2f573af0")
    , _totalReport = report1_total
    , _reports = [report1_ch1, report1_ch2, report1_ch3]
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

report1_total :: TotalReport
report1_total =
  TotalReport
    { _indications =
        [ LevelsAnsweredIndication {_answeredQuestions = 5, _unansweredQuestions = 1}
        , AnsweredIndication {_answeredQuestions = 12, _unansweredQuestions = 1}
        ]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricI ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Just 1.0}
        ]
    }

report1_total_full :: TotalReport
report1_total_full =
  TotalReport
    { _indications =
        [ LevelsAnsweredIndication {_answeredQuestions = 5, _unansweredQuestions = 1}
        , AnsweredIndication {_answeredQuestions = 12, _unansweredQuestions = 1}
        ]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricI ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_total_full_disabled_levels :: TotalReport
report1_total_full_disabled_levels =
  TotalReport
    { _indications = [AnsweredIndication {_answeredQuestions = 12, _unansweredQuestions = 1}]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricI ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch1 :: ChapterReport
report1_ch1 =
  ChapterReport
    { _uuid = chapter1 ^. uuid
    , _indications =
        [ LevelsAnsweredIndication {_answeredQuestions = 1, _unansweredQuestions = 0}
        , AnsweredIndication {_answeredQuestions = 3, _unansweredQuestions = 0}
        ]
    , _metrics =
        [ MetricSummary {_uuid = metricI ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Just 1.0}
        ]
    }

report1_ch1_full :: ChapterReport
report1_ch1_full =
  ChapterReport
    { _uuid = report1_ch1 ^. chapterUuid
    , _indications = report1_ch1 ^. indications
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Nothing}
        ] ++
        (report1_ch1 ^. metrics) ++
        [ MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch1_full_disabled_levels :: ChapterReport
report1_ch1_full_disabled_levels =
  ChapterReport
    { _uuid = report1_ch1 ^. chapterUuid
    , _indications = [AnsweredIndication {_answeredQuestions = 3, _unansweredQuestions = 0}]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Nothing}
        ] ++
        (report1_ch1 ^. metrics) ++
        [ MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch2 :: ChapterReport
report1_ch2 =
  ChapterReport
    { _uuid = chapter2 ^. uuid
    , _indications =
        [ LevelsAnsweredIndication {_answeredQuestions = 2, _unansweredQuestions = 1}
        , AnsweredIndication {_answeredQuestions = 7, _unansweredQuestions = 1}
        ]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Just 1.0}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Just 1.0}
        ]
    }

report1_ch2_full :: ChapterReport
report1_ch2_full =
  ChapterReport
    { _uuid = report1_ch2 ^. chapterUuid
    , _indications = report1_ch2 ^. indications
    , _metrics =
        (report1_ch2 ^. metrics) ++
        [ MetricSummary {_uuid = metricI ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch2_full_disabled_levels :: ChapterReport
report1_ch2_full_disabled_levels =
  ChapterReport
    { _uuid = report1_ch2 ^. chapterUuid
    , _indications = [AnsweredIndication {_answeredQuestions = 7, _unansweredQuestions = 1}]
    , _metrics =
        (report1_ch2 ^. metrics) ++
        [ MetricSummary {_uuid = metricI ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch3 :: ChapterReport
report1_ch3 =
  ChapterReport
    { _uuid = chapter3 ^. uuid
    , _indications =
        [ LevelsAnsweredIndication {_answeredQuestions = 2, _unansweredQuestions = 0}
        , AnsweredIndication {_answeredQuestions = 2, _unansweredQuestions = 0}
        ]
    , _metrics = []
    }

report1_ch3_full :: ChapterReport
report1_ch3_full =
  ChapterReport
    { _uuid = report1_ch3 ^. chapterUuid
    , _indications = report1_ch3 ^. indications
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricI ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }

report1_ch3_full_disabled_levels :: ChapterReport
report1_ch3_full_disabled_levels =
  ChapterReport
    { _uuid = report1_ch3 ^. chapterUuid
    , _indications = [AnsweredIndication {_answeredQuestions = 2, _unansweredQuestions = 0}]
    , _metrics =
        [ MetricSummary {_uuid = metricF ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricA ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricI ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricR ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricG ^. uuid, _measure = Nothing}
        , MetricSummary {_uuid = metricO ^. uuid, _measure = Nothing}
        ]
    }
