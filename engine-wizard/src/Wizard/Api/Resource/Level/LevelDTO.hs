module Wizard.Api.Resource.Level.LevelDTO where

import Data.Time
import GHC.Generics

data LevelDTO =
  LevelDTO
    { _level :: Int
    , _title :: String
    , _description :: Maybe String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq LevelDTO where
  a == b = _level a == _level b && _title a == _title b && _description a == _description b
