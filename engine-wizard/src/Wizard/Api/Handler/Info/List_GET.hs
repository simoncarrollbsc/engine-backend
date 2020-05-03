module Wizard.Api.Handler.Info.List_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext

type List_GET = Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] InfoDTO)

list_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] InfoDTO)
list_GET =
  runInUnauthService $
  addTraceUuidHeader =<< do
    context <- ask
    let buildInfoConfig_ = context ^. buildInfoConfig
    return
      InfoDTO
        { _name = buildInfoConfig_ ^. name
        , _version = buildInfoConfig_ ^. version
        , _builtAt = buildInfoConfig_ ^. builtAt
        }
