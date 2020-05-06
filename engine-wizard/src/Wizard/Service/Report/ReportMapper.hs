module Wizard.Service.Report.ReportMapper where

import Control.Lens ((^.))
import Data.Maybe (mapMaybe)

import LensesConfig
import LensesExtension
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Model.Report.Report

toIndicationDTO :: Indication -> IndicationDTO
toIndicationDTO ai@AnsweredIndication {} =
  AnsweredIndicationDTO
      { _answeredIndicationDTOAnsweredQuestions = ai ^. answeredQuestions'
      , _answeredIndicationDTOUnansweredQuestions = ai ^. unansweredQuestions'
      }
toIndicationDTO ai@LevelsAnsweredIndication {} =
  LevelsAnsweredIndicationDTO
    { _levelsAnsweredIndicationDTOAnsweredQuestions = ai ^. answeredQuestions'
    , _levelsAnsweredIndicationDTOUnansweredQuestions = ai ^. unansweredQuestions'
    }

toMetricSummaryDTO :: MetricSummary -> Maybe MetricSummaryDTO
toMetricSummaryDTO ms =
  case ms ^. measure of
    Just msMeasure ->
      Just MetricSummaryDTO {_metricSummaryDTOMetricUuid = ms ^. metricUuid, _metricSummaryDTOMeasure = msMeasure}
    Nothing -> Nothing

toChapterReportDTO :: ChapterReport -> ChapterReportDTO
toChapterReportDTO chr =
  ChapterReportDTO
    { _chapterReportDTOChapterUuid = chr ^. chapterUuid
    , _chapterReportDTOIndications = toIndicationDTO <$> chr ^. indications
    , _chapterReportDTOMetrics = mapMaybe toMetricSummaryDTO $ chr ^. metrics
    }

toTotalReportDTO :: TotalReport -> TotalReportDTO
toTotalReportDTO tr =
  TotalReportDTO
    { _totalReportDTOIndications = toIndicationDTO <$> tr ^. indications
    , _totalReportDTOMetrics = mapMaybe toMetricSummaryDTO $ tr ^. metrics
    }

toReportDTO :: Report -> ReportDTO
toReportDTO r =
  ReportDTO
    { _reportDTOUuid = r ^. uuid
    , _reportDTOTotalReport = toTotalReportDTO $ r ^. totalReport
    , _reportDTOChapterReports = toChapterReportDTO <$> r ^. chapterReports
    , _reportDTOCreatedAt = r ^. createdAt
    , _reportDTOUpdatedAt = r ^. updatedAt
    }
