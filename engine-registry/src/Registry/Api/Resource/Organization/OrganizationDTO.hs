module Registry.Api.Resource.Organization.OrganizationDTO where

import Data.Time
import GHC.Generics

import qualified Registry.Model.Organization.Organization as Organization

data OrganizationDTO =
  OrganizationDTO
    { _organizationId :: String
    , _name :: String
    , _description :: String
    , _email :: String
    , _role :: Organization.OrganizationRole
    , _token :: String
    , _active :: Bool
    , _logo :: Maybe String
    , _createdAt :: UTCTime
    , _updatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq OrganizationDTO where
  a == b =
    _organizationId a == _organizationId b &&
    _name a == _name b &&
    _description a == _description b &&
    _email a == _email b && _role a == _role b && _token a == _token b && _active a == _active b && _logo a == _logo b
