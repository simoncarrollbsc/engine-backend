module Wizard.Integration.Http.Registry.RequestMapper where

import Control.Lens ((^.))
import Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Prelude hiding (lookup)
import Servant
import Servant.Client

import LensesConfig
import Registry.Api.Handler.Organization.Detail_State_PUT
import Registry.Api.Handler.Organization.List_POST
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Constant.Api
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrieveOrganizationsRequest :: ServerConfigRegistry -> AppConfigRegistry -> HttpRequest
toRetrieveOrganizationsRequest serverConfig appConfig =
  HttpRequest
    { _requestMethod = "GET"
    , _requestUrl = serverConfig ^. url ++ "/organizations/simple"
    , _requestHeaders = M.empty
    , _requestBody = BS.empty
    , _multipartFileName = Nothing
    }

toCreateOrganizationRequest ::
     ServerConfig -> OrganizationCreateDTO -> ClientM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
toCreateOrganizationRequest serverConfig reqDto =
  client list_POST_Api reqDto (Just $ serverConfig ^. general . clientUrl)

toConfirmOrganizationRegistrationRequest ::
     ServerConfig -> RegistryConfirmationDTO -> ClientM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
toConfirmOrganizationRegistrationRequest serverConfig reqDto =
  client detail_state_PUT_Api (OrganizationStateDTO {_active = True}) (reqDto ^. organizationId) (Just $ reqDto ^. hash)

toRetrievePackagesRequest :: ServerConfigRegistry -> AppConfigRegistry -> InstanceStatistics -> HttpRequest
toRetrievePackagesRequest serverConfig appConfig iStat =
  HttpRequest
    { _requestMethod = "GET"
    , _requestUrl = serverConfig ^. url ++ "/packages"
    , _requestHeaders =
        M.fromList
          [ (authorizationHeaderName, "Bearer " ++ appConfig ^. token)
          , (xUserCountHeaderName, show $ iStat ^. userCount)
          , (xPkgCountHeaderName, show $ iStat ^. pkgCount)
          , (xQtnCountHeaderName, show $ iStat ^. qtnCount)
          ]
    , _requestBody = BS.empty
    , _multipartFileName = Nothing
    }

toRetrievePackageBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest serverConfig appConfig pkgId =
  HttpRequest
    { _requestMethod = "GET"
    , _requestUrl = serverConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
    , _requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token)]
    , _requestBody = BS.empty
    , _multipartFileName = Nothing
    }
