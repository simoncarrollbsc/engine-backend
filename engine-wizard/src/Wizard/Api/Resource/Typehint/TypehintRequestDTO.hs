module Wizard.Api.Resource.Typehint.TypehintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data TypehintRequestDTO =
  TypehintRequestDTO
    { _packageId :: Maybe String
    , _events :: [Event]
    , _questionUuid :: U.UUID
    , _q :: String
    }
  deriving (Show, Eq, Generic)
