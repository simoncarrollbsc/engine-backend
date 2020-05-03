module Registry.Model.Organization.Organization where

import Data.Time
import GHC.Generics

data OrganizationRole
  = AdminRole
  | UserRole
  deriving (Show, Eq, Generic)

data Organization =
  Organization
    { _organizationId :: String
    , _name :: String
    , _description :: String
    , _email :: String
    , _role :: OrganizationRole
    , _token :: String
    , _active :: Bool
    , _logo :: Maybe String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq Organization where
  a == b =
    _organizationId a == _organizationId b &&
    _name a == _name b &&
    _description a == _description b &&
    _email a == _email b && _role a == _role b && _token a == _token b && _active a == _active b && _logo a == _logo b
