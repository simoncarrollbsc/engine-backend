module Wizard.Api.Resource.BookReference.BookReferenceDTO where

import Data.Time
import GHC.Generics

data BookReferenceDTO =
  BookReferenceDTO
    { _shortUuid :: String
    , _bookChapter :: String
    , _content :: String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq BookReferenceDTO where
  a == b = _shortUuid a == _shortUuid b && _content a == _content b
