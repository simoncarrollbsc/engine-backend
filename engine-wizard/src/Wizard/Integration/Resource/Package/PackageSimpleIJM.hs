module Wizard.Integration.Resource.Package.PackageSimpleIJM where

import Control.Monad
import Data.Aeson

import Wizard.Integration.Resource.Organization.OrganizationSimpleIJM ()
import Wizard.Integration.Resource.Package.PackageSimpleIDTO

instance ToJSON PackageSimpleIDTO where
  toJSON PackageSimpleIDTO {..} =
    object
      [ "id" .= _pId
      , "name" .= _name
      , "organizationId" .= _organizationId
      , "kmId" .= _kmId
      , "version" .= _version
      , "description" .= _description
      , "organization" .= _organization
      , "createdAt" .= _createdAt
      ]

instance FromJSON PackageSimpleIDTO where
  parseJSON (Object o) = do
    _pId <- o .: "id"
    _name <- o .: "name"
    _organizationId <- o .: "organizationId"
    _kmId <- o .: "kmId"
    _version <- o .: "version"
    _description <- o .: "description"
    _organization <- o .: "organization"
    _createdAt <- o .: "createdAt"
    return PackageSimpleIDTO {..}
  parseJSON _ = mzero
