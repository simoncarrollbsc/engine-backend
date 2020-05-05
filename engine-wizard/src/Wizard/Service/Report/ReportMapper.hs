module Wizard.Service.Report.ReportMapper where

import Control.Lens ((^.))
import Data.Maybe (mapMaybe)

import LensesConfig
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Model.Report.Report

toIndicationDTO :: Indication -> IndicationDTO
toIndicationDTO ai@AnsweredIndication {} =
  AnsweredIndicationDTO {_answeredQuestions = ai ^. answeredQuestions, _unansweredQuestions = ai ^. unansweredQuestions}
toIndicationDTO ai@LevelsAnsweredIndication {} =
  LevelsAnsweredIndicationDTO
    {_answeredQuestions = ai ^. answeredQuestions, _unansweredQuestions = ai ^. unansweredQuestions}

toMetricSummaryDTO :: MetricSummary -> Maybe MetricSummaryDTO
toMetricSummaryDTO ms =
  case ms ^. measure of
    Just msMeasure -> Just MetricSummaryDTO {_uuid = ms ^. metricUuid, _measure = msMeasure}
    Nothing -> Nothing

toChapterReportDTO :: ChapterReport -> ChapterReportDTO
toChapterReportDTO chr =
  ChapterReportDTO
    { _uuid = chr ^. chapterUuid
    , _indications = toIndicationDTO <$> chr ^. indications
    , _metrics = mapMaybe toMetricSummaryDTO $ chr ^. metrics
    }

toTotalReportDTO :: TotalReport -> TotalReportDTO
toTotalReportDTO tr =
  TotalReportDTO
    {_indications = toIndicationDTO <$> tr ^. indications, _metrics = mapMaybe toMetricSummaryDTO $ tr ^. metrics}

toReportDTO :: Report -> ReportDTO
toReportDTO r =
  ReportDTO
    { _uuid = r ^. uuid
    , _totalReport = toTotalReportDTO $ r ^. totalReport
    , _reports = toChapterReportDTO <$> r ^. chapterReports
    , _createdAt = r ^. createdAt
    , _updatedAt = r ^. updatedAt
    }
