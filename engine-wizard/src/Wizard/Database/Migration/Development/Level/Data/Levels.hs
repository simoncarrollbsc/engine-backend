module Wizard.Database.Migration.Development.Level.Data.Levels where

import Data.Maybe (fromJust)
import Data.Time

import Wizard.Model.Level.Level

level1 :: Level
level1 =
  Level
    { _level = 1
    , _title = "Before Submitting the Proposal"
    , _description = Just ""
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

level2 :: Level
level2 =
  Level
    { _level = 2
    , _title = "Before Submitting the DMP"
    , _description = Just ""
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

level3 :: Level
level3 =
  Level
    { _level = 3
    , _title = "Before Finishing the Project"
    , _description = Just ""
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
