module Shared.Api.Resource.Event.ExpertEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Api.Resource.Event.ExpertEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddExpertEventDTO where
  declareNamedSchema = simpleToSchema'' "_addExpertEventDTO" "type" (toDTO a_km1_ch1_q2_eAlbert)

instance ToSchema EditExpertEventDTO where
  declareNamedSchema = simpleToSchema'' "_editExpertEventDTO" "type" (toDTO e_km1_ch1_q2_eAlbert)

instance ToSchema DeleteExpertEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteExpertEventDTO" "type" (toDTO d_km1_ch1_q2_eNikola)
