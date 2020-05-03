module Registry.Api.Handler.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import Network.HTTP.Types.Status
import Servant
  ( Header
  , Headers
  , ServerError(..)
  , addHeader
  , err302
  , err400
  , err401
  , err401
  , err403
  , err404
  , err500
  , errBody
  , errHeaders
  , throwError
  )

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Internal
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Util.Logger
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Locale
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid

runInUnauthService :: AppContextM a -> BaseContextM a
runInUnauthService function = do
  traceUuid_ <- liftIO generateUuid
  context <- ask
  let serverConfig_ = context ^. serverConfig
  let localization_ = context ^. localization
  let buildInfoConfig_ = context ^. buildInfoConfig
  let dbPool_ = context ^. pool
  let appContext =
        AppContext
          { _serverConfig = serverConfig_
          , _localization = localization_
          , _buildInfoConfig = buildInfoConfig_
          , _pool = dbPool_
          , _traceUuid = traceUuid_
          , _currentOrganization = Nothing
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

runInAuthService :: Organization -> AppContextM a -> BaseContextM a
runInAuthService organization function = do
  traceUuid_ <- liftIO generateUuid
  context <- ask
  let serverConfig_ = context ^. serverConfig
  let localization_ = context ^. localization
  let buildInfoConfig_ = context ^. buildInfoConfig
  let dbPool_ = context ^. pool
  let appContext =
        AppContext
          { _serverConfig = serverConfig_
          , _localization = localization_
          , _buildInfoConfig = buildInfoConfig_
          , _pool = dbPool_
          , _traceUuid = traceUuid_
          , _currentOrganization = Just organization
          }
  eResult <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  case eResult of
    Right result -> return result
    Left error -> do
      dto <- sendError error
      throwError dto

getMaybeAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  organization <- getCurrentOrganization tokenHeader
  callback (runInAuthService organization)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((AppContextM a -> BaseContextM a) -> BaseContextM b) -> BaseContextM b
getAuthServiceExecutor (Just token) callback = do
  org <- getCurrentOrganization token
  callback (runInAuthService org)
getAuthServiceExecutor Nothing _ = do
  dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
  throwError dto

getCurrentOrganization :: String -> BaseContextM Organization
getCurrentOrganization tokenHeader = do
  orgToken <- getCurrentOrgToken tokenHeader
  mOrg <- runInUnauthService (findOrganizationByToken' orgToken)
  case mOrg of
    Just org -> return org
    Nothing -> do
      dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION
      throwError dto

getCurrentOrgToken :: String -> BaseContextM String
getCurrentOrgToken tokenHeader = do
  let orgTokenMaybe = separateToken tokenHeader
  case orgTokenMaybe of
    Just orgToken -> return orgToken
    Nothing -> do
      dto <- sendErrorDTO $ UnauthorizedErrorDTO _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      throwError dto

addTraceUuidHeader :: a -> AppContextM (Headers '[ Header "x-trace-uuid" String] a)
addTraceUuidHeader result = do
  context <- ask
  let traceUuid_ = context ^. traceUuid
  return $ addHeader (U.toString traceUuid_) result

sendError :: AppError -> BaseContextM ServerError
sendError AcceptedError =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendError (FoundError url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendError (ValidationError formErrorRecords fieldErrorRecords) = do
  context <- ask
  let ls = context ^. localization
  let formErrors = fmap (locale ls) formErrorRecords
  let localeTuple (k, v) = (k, locale ls v)
  let fieldErrors = fmap localeTuple fieldErrorRecords
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendError (UserError localeRecord) = do
  context <- ask
  let ls = context ^. localization
  let message = locale ls localeRecord
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (UnauthorizedError localeRecord) = do
  context <- ask
  let ls = context ^. localization
  let message = locale ls localeRecord
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError localeRecord) = do
  context <- ask
  let ls = context ^. localization
  let message = locale ls localeRecord
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError localeRecord) = do
  context <- ask
  let ls = context ^. localization
  let message = locale ls localeRecord
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (GeneralServerError errorMessage) = do
  logError _CMP_API errorMessage
  return $ err500 {errBody = encode $ GeneralServerErrorDTO errorMessage, errHeaders = [contentTypeHeaderJSON]}
sendError (HttpClientError status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }

sendErrorDTO :: ErrorDTO -> BaseContextM ServerError
sendErrorDTO AcceptedErrorDTO =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendErrorDTO (FoundErrorDTO url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendErrorDTO (ValidationErrorDTO formErrors fieldErrors) =
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UserErrorDTO message) =
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UnauthorizedErrorDTO message) =
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (ForbiddenErrorDTO message) =
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (NotExistsErrorDTO message) =
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (GeneralServerErrorDTO message) = do
  logError _CMP_API message
  return $ err500 {errBody = encode $ GeneralServerErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (HttpClientErrorDTO status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }
