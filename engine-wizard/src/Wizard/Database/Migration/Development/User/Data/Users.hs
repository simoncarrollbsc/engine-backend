module Wizard.Database.Migration.Development.User.Data.Users where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Common.SensitiveData
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Service.User.UserProfileMapper

userAlbert :: User
userAlbert =
  User
    { _uuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , _firstName = "Albert"
    , _lastName = "Einstein"
    , _email = "albert.einstein@example.com"
    , _affiliation = Just "My University"
    , _sources = [_USER_SOURCE_INTERNAL]
    , _role = _USER_ROLE_ADMIN
    , _permissions =
        [ "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        ]
    , _active = True
    , _passwordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _submissionProps = [userAlbertApiTokenEncrypted]
    , _imageUrl = Nothing
    , _createdAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userAlbertEdited :: User
userAlbertEdited =
  userAlbert
    { _firstName = "EDITED: Isaac"
    , _lastName = "EDITED: Einstein"
    , _email = "albert.einstein@example-edited.com"
    , _affiliation = Just "EDITED: My University"
    , _submissionProps = [userAlbertApiTokenEditedEncrypted]
    }

userAlbertDecrypted :: User
userAlbertDecrypted = process defaultSecret userAlbert

userNikola :: User
userNikola =
  User
    { _uuid = fromJust . U.fromString $ "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , _firstName = "Nikola"
    , _lastName = "Tesla"
    , _email = "nikola.tesla@example.com"
    , _affiliation = Nothing
    , _sources = [_USER_SOURCE_INTERNAL]
    , _role = _USER_ROLE_DATA_STEWARD
    , _permissions =
        ["KM_PERM", "KM_UPGRADE_PERM", "KM_PUBLISH_PERM", "PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , _active = True
    , _passwordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _submissionProps = []
    , _imageUrl = Nothing
    , _createdAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userIsaac :: User
userIsaac =
  User
    { _uuid = fromJust . U.fromString $ "e1c58e52-0824-4526-8ebe-ec38eec67030"
    , _firstName = "Isaac"
    , _lastName = "Newton"
    , _email = "isaac.newton@example.com"
    , _affiliation = Nothing
    , _sources = [_USER_SOURCE_INTERNAL]
    , _role = _USER_ROLE_RESEARCHER
    , _permissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , _active = True
    , _passwordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _submissionProps = []
    , _imageUrl = Nothing
    , _createdAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
    { _firstName = "John"
    , _lastName = "Doe"
    , _email = "john.doe@example.com"
    , _affiliation = Just "My University"
    , _role = Just _USER_ROLE_ADMIN
    , _password = "password"
    }

userIsaacChange :: UserChangeDTO
userIsaacChange =
  UserChangeDTO
    { _uuid = userAlbert ^. uuid
    , _firstName = "EDITED: Isaac"
    , _lastName = "EDITED: Newton"
    , _email = "albert.einstein@example.com"
    , _affiliation = Just "EDITED: My University"
    , _role = _USER_ROLE_ADMIN
    , _active = True
    }

userAlbertProfile :: UserProfileDTO
userAlbertProfile = toUserProfileDTO userAlbert [userAlbertApiTokenDto]

userAlbertProfileEdited :: UserProfileDTO
userAlbertProfileEdited =
  userAlbertProfile
    { _firstName = userAlbertEdited ^. firstName
    , _lastName = userAlbertEdited ^. lastName
    , _email = userAlbertEdited ^. email
    , _affiliation = userAlbertEdited ^. affiliation
    , _submissionProps = [userAlbertApiTokenEditedDto]
    }

userIsaacProfileChange :: UserProfileChangeDTO
userIsaacProfileChange =
  UserProfileChangeDTO
    { _firstName = userAlbertEdited ^. firstName
    , _lastName = userAlbertEdited ^. lastName
    , _email = userAlbertEdited ^. email
    , _affiliation = userAlbertEdited ^. affiliation
    , _submissionProps = [toUserSubmissionPropsDTO userAlbertApiTokenEdited (defaultSubmissionService ^. name)]
    }

userPassword :: UserPasswordDTO
userPassword = UserPasswordDTO {_password = "newPassword"}

userState :: UserStateDTO
userState = UserStateDTO {_active = True}

userAlbertApiToken :: UserSubmissionProps
userAlbertApiToken =
  UserSubmissionProps
    { _sId = defaultSubmissionService ^. sId
    , _values = M.fromList [(defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenEncrypted :: UserSubmissionProps
userAlbertApiTokenEncrypted = process defaultSecret userAlbertApiToken

userAlbertApiTokenDto :: UserSubmissionPropsDTO
userAlbertApiTokenDto =
  UserSubmissionPropsDTO
    { _sId = defaultSubmissionService ^. sId
    , _name = defaultSubmissionService ^. name
    , _values =
        M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenEdited :: UserSubmissionProps
userAlbertApiTokenEdited =
  userAlbertApiToken {_values = M.fromList [(defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]}

userAlbertApiTokenEditedEncrypted :: UserSubmissionProps
userAlbertApiTokenEditedEncrypted = process defaultSecret userAlbertApiTokenEdited

userAlbertApiTokenEditedDto :: UserSubmissionPropsDTO
userAlbertApiTokenEditedDto =
  UserSubmissionPropsDTO
    { _sId = defaultSubmissionService ^. sId
    , _name = defaultSubmissionService ^. name
    , _values =
        M.fromList
          [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }
