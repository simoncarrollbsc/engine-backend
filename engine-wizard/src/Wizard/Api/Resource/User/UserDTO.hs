module Wizard.Api.Resource.User.UserDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsJM ()
import Wizard.Model.Acl.Acl

data UserDTO =
  UserDTO
    { _userDTOUuid :: UUID
    , _userDTOFirstName :: String
    , _userDTOLastName :: String
    , _userDTOEmail :: String
    , _userDTOAffiliation :: Maybe String
    , _userDTOSources :: [String]
    , _userDTORole :: String
    , _userDTOPermissions :: [String]
    , _userDTOActive :: Bool
    , _userDTOImageUrl :: Maybe String
    , _userDTOGroups :: [GroupMembership]
    , _userDTOCreatedAt :: Maybe UTCTime
    , _userDTOUpdatedAt :: Maybe UTCTime
    }
  deriving (Show, Generic)

instance Eq UserDTO where
  a == b =
    _userDTOUuid a == _userDTOUuid b &&
    _userDTOFirstName a == _userDTOFirstName b &&
    _userDTOLastName a == _userDTOLastName b &&
    _userDTOEmail a == _userDTOEmail b &&
    _userDTOAffiliation a == _userDTOAffiliation b &&
    _userDTOSources a == _userDTOSources b &&
    _userDTORole a == _userDTORole b &&
    _userDTOPermissions a == _userDTOPermissions b &&
    _userDTOActive a == _userDTOActive b &&
    _userDTOImageUrl a == _userDTOImageUrl b && _userDTOGroups a == _userDTOGroups b
