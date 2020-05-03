module Registry.Model.Audit.AuditEntry where

import Data.Time
import GHC.Generics

import Registry.Model.Statistics.InstanceStatistics

data AuditEntry
  = ListPackagesAuditEntry
      { _organizationId :: String
      , _instanceStatistics :: InstanceStatistics
      , _createdAt :: UTCTime
      }
  | GetPackageBundleAuditEntry
      { _organizationId :: String
      , _packageId :: String
      , _createdAt :: UTCTime
      }
  deriving (Show, Generic)

instance Eq AuditEntry where
  ae1@ListPackagesAuditEntry {} == ae2@ListPackagesAuditEntry {} =
    _organizationId ae1 == _organizationId ae2 && _instanceStatistics ae1 == _instanceStatistics ae2
  ae1@GetPackageBundleAuditEntry {} == ae2@GetPackageBundleAuditEntry {} =
    _organizationId ae1 == _organizationId ae2 && _packageId ae1 == _packageId ae2
