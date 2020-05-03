module Shared.Database.Migration.Development.Info.Data.Infos where

import Shared.Api.Resource.Info.InfoDTO

appInfo :: InfoDTO
appInfo = InfoDTO {_name = "Engine", _version = "1.0.0", _builtAt = "2017/10/25 19:50:20Z"}
