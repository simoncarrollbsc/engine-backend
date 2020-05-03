module Wizard.Model.BookReference.BookReference where

import Data.Time
import GHC.Generics

data BookReference =
  BookReference
    { _shortUuid :: String
    , _bookChapter :: String
    , _content :: String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq BookReference where
  a == b = _shortUuid a == _shortUuid b && _bookChapter a == _bookChapter b && _content a == _content b
