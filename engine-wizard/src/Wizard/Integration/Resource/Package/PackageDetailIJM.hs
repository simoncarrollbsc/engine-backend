module Wizard.Integration.Resource.Package.PackageDetailIJM where

import Control.Monad
import Data.Aeson

import Wizard.Integration.Resource.Organization.OrganizationSimpleIJM ()
import Wizard.Integration.Resource.Package.PackageDetailIDTO

instance ToJSON PackageDetailIDTO where
  toJSON PackageDetailIDTO {..} =
    object
      [ "id" .= _pId
      , "name" .= _name
      , "organizationId" .= _organizationId
      , "kmId" .= _kmId
      , "version" .= _version
      , "metamodelVersion" .= _metamodelVersion
      , "description" .= _description
      , "readme" .= _readme
      , "license" .= _license
      , "previousPackageId" .= _previousPackageId
      , "forkOfPackageId" .= _forkOfPackageId
      , "mergeCheckpointPackageId" .= _mergeCheckpointPackageId
      , "versions" .= _versions
      , "organization" .= _organization
      , "createdAt" .= _createdAt
      ]

instance FromJSON PackageDetailIDTO where
  parseJSON (Object o) = do
    _pId <- o .: "id"
    _name <- o .: "name"
    _organizationId <- o .: "organizationId"
    _kmId <- o .: "kmId"
    _version <- o .: "version"
    _metamodelVersion <- o .: "metamodelVersion"
    _description <- o .: "description"
    _readme <- o .: "readme"
    _license <- o .: "license"
    _previousPackageId <- o .: "previousPackageId"
    _forkOfPackageId <- o .: "forkOfPackageId"
    _mergeCheckpointPackageId <- o .: "mergeCheckpointPackageId"
    _versions <- o .: "versions"
    _organization <- o .: "organization"
    _createdAt <- o .: "createdAt"
    return PackageDetailIDTO {..}
  parseJSON _ = mzero
