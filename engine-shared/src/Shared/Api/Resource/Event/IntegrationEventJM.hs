module Shared.Api.Resource.Event.IntegrationEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Util.JSON

instance FromJSON AddIntegrationEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddIntegrationEventDTO where
  toJSON = simpleToJSON' "_addIntegrationEventDTO" "type"

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditIntegrationEventDTO where
  toJSON = simpleToJSON' "_editIntegrationEventDTO" "type"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DeleteIntegrationEventDTO where
  toJSON = simpleToJSON' "_deleteIntegrationEventDTO" "type"
