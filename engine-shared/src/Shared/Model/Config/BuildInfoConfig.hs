module Shared.Model.Config.BuildInfoConfig where

import GHC.Generics

data BuildInfoConfig =
  BuildInfoConfig
    { _name :: String
    , _version :: String
    , _builtAt :: String
    }
  deriving (Generic, Show)
