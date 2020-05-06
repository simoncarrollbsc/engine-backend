module Shared.Api.Resource.Event.EventFieldJM where

import Data.Aeson
import Control.Monad

import Shared.Model.Event.EventField
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance FromJSON a => FromJSON (EventField a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValue efValue
      else return NothingChanged
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventField a) where
  toJSON (ChangedValue efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChanged = object ["changed" .= False]

