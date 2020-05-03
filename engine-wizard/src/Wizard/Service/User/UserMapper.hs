module Wizard.Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Char (toLower)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.User.User

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
    { _uuid = user ^. uuid
    , _firstName = user ^. firstName
    , _lastName = user ^. lastName
    , _email = user ^. email
    , _affiliation = user ^. affiliation
    , _sources = user ^. sources
    , _role = user ^. role
    , _permissions = user ^. permissions
    , _active = user ^. active
    , _submissionProps = user ^. submissionProps
    , _imageUrl = user ^. imageUrl
    , _createdAt = user ^. createdAt
    , _updatedAt = user ^. updatedAt
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> UTCTime -> UTCTime -> User
fromUserCreateDTO dto userUuid passwordHash role permissions createdAt updatedAt =
  User
    { _uuid = userUuid
    , _firstName = dto ^. firstName
    , _lastName = dto ^. lastName
    , _email = toLower <$> dto ^. email
    , _passwordHash = passwordHash
    , _affiliation = dto ^. affiliation
    , _sources = [_USER_SOURCE_INTERNAL]
    , _role = role
    , _permissions = permissions
    , _active = False
    , _submissionProps = []
    , _imageUrl = Nothing
    , _createdAt = Just createdAt
    , _updatedAt = Just updatedAt
    }

fromUserExternalDTO ::
     U.UUID
  -> String
  -> String
  -> String
  -> String
  -> [String]
  -> Role
  -> [Permission]
  -> Maybe String
  -> UTCTime
  -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash sources role permissions mImageUrl now =
  User
    { _uuid = userUuid
    , _firstName = firstName
    , _lastName = lastName
    , _email = email
    , _passwordHash = passwordHash
    , _affiliation = Nothing
    , _sources = sources
    , _role = role
    , _permissions = permissions
    , _active = True
    , _submissionProps = []
    , _imageUrl = mImageUrl
    , _createdAt = Just now
    , _updatedAt = Just now
    }

fromUpdateUserExternalDTO :: User -> String -> String -> Maybe String -> String -> UTCTime -> User
fromUpdateUserExternalDTO oldUser firstName lastName mImageUrl serviceId now =
  User
    { _uuid = oldUser ^. uuid
    , _firstName = firstName
    , _lastName = lastName
    , _email = oldUser ^. email
    , _passwordHash = oldUser ^. passwordHash
    , _affiliation = oldUser ^. affiliation
    , _sources =
        case L.find (== serviceId) (oldUser ^. sources) of
          Just _ -> oldUser ^. sources
          Nothing -> (oldUser ^. sources) ++ [serviceId]
    , _role = oldUser ^. role
    , _permissions = oldUser ^. permissions
    , _active = oldUser ^. active
    , _submissionProps = oldUser ^. submissionProps
    , _imageUrl = mImageUrl
    , _createdAt = oldUser ^. createdAt
    , _updatedAt = oldUser ^. updatedAt
    }

fromUserChangeDTO :: UserChangeDTO -> User -> [Permission] -> User
fromUserChangeDTO dto oldUser permission =
  User
    { _uuid = oldUser ^. uuid
    , _firstName = dto ^. firstName
    , _lastName = dto ^. lastName
    , _email = toLower <$> dto ^. email
    , _passwordHash = oldUser ^. passwordHash
    , _affiliation = dto ^. affiliation
    , _sources = oldUser ^. sources
    , _role = dto ^. role
    , _permissions = permission
    , _active = dto ^. active
    , _submissionProps = oldUser ^. submissionProps
    , _imageUrl = oldUser ^. imageUrl
    , _createdAt = oldUser ^. createdAt
    , _updatedAt = oldUser ^. updatedAt
    }
