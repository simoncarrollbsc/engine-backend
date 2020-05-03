module Shared.Model.PackageBundle.PackageBundle where

import GHC.Generics

import Shared.Model.Package.PackageWithEvents

data PackageBundle =
  PackageBundle
    { _bundleId :: String
    , _name :: String
    , _organizationId :: String
    , _kmId :: String
    , _version :: String
    , _metamodelVersion :: Int
    , _packages :: [PackageWithEvents]
    }
  deriving (Show, Eq, Generic)
