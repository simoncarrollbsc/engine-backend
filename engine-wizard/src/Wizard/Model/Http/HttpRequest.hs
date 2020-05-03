module Wizard.Model.Http.HttpRequest where

import Data.ByteString.Char8 as BS
import Data.Map (Map)

data HttpRequest =
  HttpRequest
    { _requestMethod :: String
    , _requestUrl :: String
    , _requestHeaders :: Map String String
    , _requestBody :: BS.ByteString
    , _multipartFileName :: Maybe String
    }
  deriving (Show, Eq)
