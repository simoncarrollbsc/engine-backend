module Wizard.Api.Resource.Typehint.TypehintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data TypehintRequestDTO =
  TypehintRequestDTO
    { _packageId :: Maybe String
    , _events :: [EventDTO]
    , _questionUuid :: U.UUID
    , _q :: String
    }
  deriving (Show, Eq, Generic)
