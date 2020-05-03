module Shared.Api.Resource.Package.PackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Package.PackageDTO
import Shared.Util.JSON

instance ToJSON PackageDTO where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    _pId <- o .: "id"
    _name <- o .: "name"
    _organizationId <- o .: "organizationId"
    _kmId <- o .: "kmId"
    _version <- o .: "version"
    _metamodelVersion <- o .: "metamodelVersion"
    _description <- o .: "description"
    _readme <- o .:? "readme" .!= ""
    _license <- o .:? "license" .!= ""
    _parentPackageId <- o .:? "parentPackageId"
    _previousPackageId <- o .:? "previousPackageId" .!= _parentPackageId
    _forkOfPackageId <- o .:? "forkOfPackageId" .!= _parentPackageId
    _mergeCheckpointPackageId <- o .:? "mergeCheckpointPackageId" .!= _parentPackageId
    eventSerialized <- o .: "events"
    _events <- parseJSON eventSerialized
    _createdAt <- o .:? "createdAt" .!= UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0
    return PackageDTO {..}
  parseJSON _ = mzero
