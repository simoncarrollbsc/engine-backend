module Wizard.Api.Resource.User.UserProfileDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User (Email, Permission, Role)

data UserProfileDTO =
  UserProfileDTO
    { _uuid :: UUID
    , _firstName :: String
    , _lastName :: String
    , _email :: Email
    , _affiliation :: Maybe String
    , _sources :: [String]
    , _role :: Role
    , _permissions :: [Permission]
    , _active :: Bool
    , _submissionProps :: [UserSubmissionPropsDTO]
    , _imageUrl :: Maybe String
    , _createdAt :: Maybe UTCTime
    , _updatedAt :: Maybe UTCTime
    }
  deriving (Show, Generic)

instance Eq UserProfileDTO where
  a == b =
    _uuid a == _uuid b &&
    _firstName a == _firstName b &&
    _lastName a == _lastName b &&
    _email a == _email b &&
    _affiliation a == _affiliation b &&
    _sources a == _sources b &&
    _role a == _role b &&
    _permissions a == _permissions b &&
    _active a == _active b && _submissionProps a == _submissionProps b && _imageUrl a == _imageUrl b
