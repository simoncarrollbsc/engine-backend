module Wizard.Messaging.Resource.Document.DocumentCreateMDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Document.DocumentContextDTO

data DocumentCreateMDTO =
  DocumentCreateMDTO
    { _documentUuid :: U.UUID
    , _documentContext :: DocumentContextDTO
    }
  deriving (Show, Eq, Generic)
