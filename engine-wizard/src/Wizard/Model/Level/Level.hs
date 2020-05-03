module Wizard.Model.Level.Level where

import Data.Time
import GHC.Generics

data Level =
  Level
    { _level :: Int
    , _title :: String
    , _description :: Maybe String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq Level where
  a == b = _level a == _level b && _title a == _title b && _description a == _description b
