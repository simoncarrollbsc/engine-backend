module Wizard.Model.User.User where

import qualified Data.Map.Strict as M
import Data.Time
import Data.UUID
import GHC.Generics

type Permission = String

type Role = String

type Email = String

_USER_SOURCE_INTERNAL = "internal"

_USER_ROLE_ADMIN = "admin"

_USER_ROLE_DATA_STEWARD = "dataSteward"

_USER_ROLE_RESEARCHER = "researcher"

data User =
  User
    { _uuid :: UUID
    , _firstName :: String
    , _lastName :: String
    , _email :: Email
    , _passwordHash :: String
    , _affiliation :: Maybe String
    , _sources :: [String]
    , _role :: Role
    , _permissions :: [Permission]
    , _active :: Bool
    , _submissionProps :: [UserSubmissionProps]
    , _imageUrl :: Maybe String
    , _createdAt :: Maybe UTCTime
    , _updatedAt :: Maybe UTCTime
    }
  deriving (Generic, Show)

data UserSubmissionProps =
  UserSubmissionProps
    { _sId :: String
    , _values :: M.Map String String
    }
  deriving (Generic, Eq, Show)

instance Eq User where
  a == b =
    _uuid a == _uuid b &&
    _firstName a == _firstName b &&
    _lastName a == _lastName b &&
    _email a == _email b &&
    _passwordHash a == _passwordHash b &&
    _affiliation a == _affiliation b &&
    _sources a == _sources b &&
    _role a == _role b &&
    _permissions a == _permissions b &&
    _active a == _active b && _submissionProps a == _submissionProps b && _imageUrl a == _imageUrl b
