module Wizard.Api.Resource.Version.VersionDTO where

import GHC.Generics

data VersionDTO =
  VersionDTO
    { _description :: String
    , _readme :: String
    , _license :: String
    }
  deriving (Generic)
