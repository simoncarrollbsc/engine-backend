module Wizard.Integration.Resource.Package.PackageSimpleIJM where

import Control.Monad
import Data.Aeson

import Wizard.Integration.Resource.Organization.OrganizationSimpleIJM ()
import Wizard.Integration.Resource.Package.PackageSimpleIDTO

instance FromJSON PackageSimpleIDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageSimpleIDTO where
  toJSON = genericToJSON simpleOptions