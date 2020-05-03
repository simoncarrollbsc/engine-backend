module Wizard.Api.Resource.Report.ReportSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Util.Swagger
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Service.Report.ReportMapper

instance ToSchema ReportDTO where
  declareNamedSchema = simpleToSchema''' (toReportDTO report1)

instance ToSchema TotalReportDTO where
  declareNamedSchema = simpleToSchema''' (toTotalReportDTO report1_total)

instance ToSchema ChapterReportDTO where
  declareNamedSchema = simpleToSchema''' (toChapterReportDTO report1_ch1)

instance ToSchema IndicationDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema MetricSummaryDTO where
  declareNamedSchema = simpleToSchema''' metricSummaryF

levelsAnsweredIndication :: IndicationDTO
levelsAnsweredIndication = LevelsAnsweredIndicationDTO {_answeredQuestions = 5, _unansweredQuestions = 1}

answeredAnsweredIndication :: IndicationDTO
answeredAnsweredIndication = AnsweredIndicationDTO {_answeredQuestions = 12, _unansweredQuestions = 1}

metricSummaryF :: MetricSummaryDTO
metricSummaryF = MetricSummaryDTO {_metricUuid = metricF ^. uuid, _measure = 1.0}
