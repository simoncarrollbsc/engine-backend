module Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures where

import Control.Lens

import LensesConfig
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- METRIC MEASURES
-- -----------------------------------------------------------------
metricMeasureF1 =
  MetricMeasure {_metricUuid = metricF ^. uuid, _measure = 1.0, _weight = 1.0}

metricMeasureA1 =
  MetricMeasure {_metricUuid = metricA ^. uuid, _measure = 1.0, _weight = 1.0}

metricMeasureI1 =
  MetricMeasure {_metricUuid = metricI ^. uuid, _measure = 1.0, _weight = 1.0}

metricMeasureR1 =
  MetricMeasure {_metricUuid = metricR ^. uuid, _measure = 1.0, _weight = 1.0}

metricMeasureG1 =
  MetricMeasure {_metricUuid = metricG ^. uuid, _measure = 1.0, _weight = 1.0}

metricMeasureO1 =
  MetricMeasure {_metricUuid = metricO ^. uuid, _measure = 1.0, _weight = 1.0}
