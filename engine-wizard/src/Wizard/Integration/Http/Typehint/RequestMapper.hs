module Wizard.Integration.Http.Typehint.RequestMapper
  ( toRetrieveTypehintsRequest
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)

toRetrieveTypehintsRequest :: Integration -> M.Map String String -> HttpRequest
toRetrieveTypehintsRequest intConfig variables =
  HttpRequest
    { _requestMethod = intConfig ^. requestMethod
    , _requestUrl = interpolateString variables (intConfig ^. requestUrl)
    , _requestHeaders = interpolateMapValues variables (intConfig ^. requestHeaders)
    , _requestBody = BS.pack $ interpolateString variables (intConfig ^. requestBody)
    , _multipartFileName = Nothing
    }
