module Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics where

import Registry.Model.Statistics.InstanceStatistics

iStat :: InstanceStatistics
iStat = InstanceStatistics {_userCount = 10, _pkgCount = 20, _qtnCount = 30}
