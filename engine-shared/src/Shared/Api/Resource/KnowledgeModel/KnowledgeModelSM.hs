module Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM where

import Control.Lens ((^.))
import Data.Swagger hiding (Reference, Tag)

import LensesConfig
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Swagger

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema KnowledgeModel where
  declareNamedSchema = simpleToSchema km1

instance ToSchema KnowledgeModelEntities where
  declareNamedSchema = simpleToSchema (km1 ^. entities)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Chapter where
  declareNamedSchema = simpleToSchema chapter1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema QuestionValueType

instance ToSchema Question where
  declareNamedSchema = simpleToSchema question1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Answer where
  declareNamedSchema = simpleToSchema q2_answerNo

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Expert where
  declareNamedSchema = simpleToSchema km1_ch1_q2_eAlbert

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Reference where
  declareNamedSchema = simpleToSchema km1_ch1_q2_r1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Metric where
  declareNamedSchema = simpleToSchema metricF

-- --------------------------------------------------------------------
instance ToSchema MetricMeasure where
  declareNamedSchema = simpleToSchema mm1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Tag where
  declareNamedSchema = simpleToSchema tagBioInformatic

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Integration where
  declareNamedSchema = simpleToSchema bioPortal