module Registry.Model.Statistics.InstanceStatistics where

import GHC.Generics

data InstanceStatistics =
  InstanceStatistics
    { _userCount :: Int
    , _pkgCount :: Int
    , _qtnCount :: Int
    }
  deriving (Show, Eq, Generic)
