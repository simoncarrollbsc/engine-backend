module Wizard.Specs.API.Config.List_Auth_PUT
  ( list_auth_PUT
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common

-- ------------------------------------------------------------------------
-- PUT /configs/auth
-- ------------------------------------------------------------------------
list_auth_PUT :: AppContext -> SpecWith Application
list_auth_PUT appContext =
  describe "PUT /configs/auth" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/configs/auth"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toAuthDTO editedAuth

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = toAuthDTO editedAuth
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfAppConfigInDB appContext (defaultAppConfig {_appConfigAuth = editedAuth})

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [reqCtHeader] reqBody "CFG_PERM"
