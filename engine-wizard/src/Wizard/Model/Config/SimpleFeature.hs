module Wizard.Model.Config.SimpleFeature where

import GHC.Generics

data SimpleFeature =
  SimpleFeature
    { _enabled :: Bool
    }
  deriving (Show, Eq, Generic)
