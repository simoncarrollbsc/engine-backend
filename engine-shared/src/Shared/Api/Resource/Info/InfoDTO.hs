module Shared.Api.Resource.Info.InfoDTO where

import GHC.Generics

data InfoDTO =
  InfoDTO
    { _name :: String
    , _version :: String
    , _builtAt :: String
    }
  deriving (Show, Eq, Generic)
