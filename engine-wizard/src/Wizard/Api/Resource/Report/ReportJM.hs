module Wizard.Api.Resource.Report.ReportJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Report.ReportDTO

-- --------------------------------------------------------------------
instance FromJSON ReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON TotalReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TotalReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON ChapterReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ChapterReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON MetricSummaryDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MetricSummaryDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON IndicationDTO where
  parseJSON = genericParseJSON (simpleOptions' "indicationType")

instance ToJSON IndicationDTO where
  toJSON = genericToJSON (simpleOptions' "indicationType")
