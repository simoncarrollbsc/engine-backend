module Wizard.Service.User.UserProfileMapper where

import Control.Lens ((^.))
import Data.Char (toLower)
import qualified Data.Map.Strict as M

import LensesConfig
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.User.User

toUserProfileDTO :: User -> [UserSubmissionPropsDTO] -> UserProfileDTO
toUserProfileDTO user props =
  UserProfileDTO
    { _uuid = user ^. uuid
    , _firstName = user ^. firstName
    , _lastName = user ^. lastName
    , _email = user ^. email
    , _affiliation = user ^. affiliation
    , _sources = user ^. sources
    , _role = user ^. role
    , _permissions = user ^. permissions
    , _active = user ^. active
    , _submissionProps = props
    , _imageUrl = user ^. imageUrl
    , _createdAt = user ^. createdAt
    , _updatedAt = user ^. updatedAt
    }

toUserSubmissionPropsDTO :: UserSubmissionProps -> String -> UserSubmissionPropsDTO
toUserSubmissionPropsDTO props name =
  UserSubmissionPropsDTO {_sId = props ^. sId, _name = name, _values = props ^. values}

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> User
fromUserProfileChangeDTO dto oldUser =
  User
    { _uuid = oldUser ^. uuid
    , _firstName = dto ^. firstName
    , _lastName = dto ^. lastName
    , _email = toLower <$> dto ^. email
    , _passwordHash = oldUser ^. passwordHash
    , _affiliation = dto ^. affiliation
    , _sources = oldUser ^. sources
    , _role = oldUser ^. role
    , _permissions = oldUser ^. permissions
    , _active = oldUser ^. active
    , _submissionProps = fromUserSubmissionPropsDTO <$> dto ^. submissionProps
    , _imageUrl = oldUser ^. imageUrl
    , _createdAt = oldUser ^. createdAt
    , _updatedAt = oldUser ^. updatedAt
    }

fromUserSubmissionPropsDTO :: UserSubmissionPropsDTO -> UserSubmissionProps
fromUserSubmissionPropsDTO dto = UserSubmissionProps {_sId = dto ^. sId, _values = dto ^. values}

fromService :: AppConfigSubmissionService -> UserSubmissionPropsDTO
fromService service =
  UserSubmissionPropsDTO
    {_sId = service ^. sId, _name = service ^. name, _values = M.fromList (fmap (\v -> (v, "")) (service ^. props))}

fromUserSubmissionProps :: UserSubmissionProps -> UserSubmissionPropsDTO
fromUserSubmissionProps props = UserSubmissionPropsDTO {_sId = props ^. sId, _name = "", _values = props ^. values}
