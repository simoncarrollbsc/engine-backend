module Registry.Service.Organization.OrganizationMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Model.Organization.Organization
import Shared.Api.Resource.Organization.OrganizationSimpleDTO

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
    { _organizationId = organization ^. organizationId
    , _name = organization ^. name
    , _description = organization ^. description
    , _email = organization ^. email
    , _role = organization ^. role
    , _token = organization ^. token
    , _logo = organization ^. logo
    , _active = organization ^. active
    , _createdAt = organization ^. createdAt
    , _updatedAt = organization ^. updatedAt
    }

toSimpleDTO :: Organization -> OrganizationSimpleDTO
toSimpleDTO organization =
  OrganizationSimpleDTO
    {_organizationId = organization ^. organizationId, _name = organization ^. name, _logo = organization ^. logo}

organizationDTOtoSimpleDTO :: OrganizationDTO -> OrganizationSimpleDTO
organizationDTOtoSimpleDTO organization =
  OrganizationSimpleDTO
    {_organizationId = organization ^. organizationId, _name = organization ^. name, _logo = organization ^. logo}

fromCreateDTO :: OrganizationCreateDTO -> OrganizationRole -> String -> UTCTime -> UTCTime -> UTCTime -> Organization
fromCreateDTO dto orgRole orgToken orgCreatedAt orgUpdatedAt orgLastAccessAt =
  Organization
    { _organizationId = dto ^. organizationId
    , _name = dto ^. name
    , _description = dto ^. description
    , _email = dto ^. email
    , _role = orgRole
    , _token = orgToken
    , _active = False
    , _logo = Nothing
    , _createdAt = orgCreatedAt
    , _updatedAt = orgUpdatedAt
    }

fromChangeDTO :: OrganizationChangeDTO -> OrganizationDTO -> UTCTime -> Organization
fromChangeDTO dto org orgUpdatedAt =
  Organization
    { _organizationId = org ^. organizationId
    , _name = dto ^. name
    , _description = dto ^. description
    , _email = dto ^. email
    , _role = org ^. role
    , _token = org ^. token
    , _active = org ^. active
    , _logo = org ^. logo
    , _createdAt = org ^. createdAt
    , _updatedAt = orgUpdatedAt
    }
