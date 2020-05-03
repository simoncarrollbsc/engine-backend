module Wizard.Api.Resource.Document.DocumentCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentCreateDTO =
  DocumentCreateDTO
    { _name :: String
    , _questionnaireUuid :: U.UUID
    , _templateUuid :: U.UUID
    , _formatUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
