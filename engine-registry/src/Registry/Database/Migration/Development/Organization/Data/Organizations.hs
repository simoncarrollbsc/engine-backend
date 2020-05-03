module Registry.Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Model.Organization.Organization
import Registry.Service.Organization.OrganizationMapper
import Shared.Database.Migration.Development.Organization.Data.Organizations

orgGlobal :: Organization
orgGlobal =
  Organization
    { _organizationId = "global"
    , _name = "Organization"
    , _description = "Some description of Organization"
    , _email = "organization@example.com"
    , _role = AdminRole
    , _token = "GlobalToken"
    , _active = True
    , _logo = Just orgLogo
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgGlobalDTO :: OrganizationDTO
orgGlobalDTO = toDTO orgGlobal

orgGlobalCreate :: OrganizationCreateDTO
orgGlobalCreate =
  OrganizationCreateDTO
    { _organizationId = orgGlobal ^. organizationId
    , _name = orgGlobal ^. name
    , _description = orgGlobal ^. description
    , _email = orgGlobal ^. email
    }

orgGlobalEdited :: Organization
orgGlobalEdited =
  Organization
    { _organizationId = orgGlobal ^. organizationId
    , _name = "EDITED: Organization"
    , _description = "EDITED: Some description of Organization"
    , _email = "edited-organization@example.com"
    , _role = orgGlobal ^. role
    , _token = orgGlobal ^. token
    , _active = orgGlobal ^. active
    , _logo = orgGlobal ^. logo
    , _createdAt = orgGlobal ^. createdAt
    , _updatedAt = orgGlobal ^. updatedAt
    }

orgGlobalEditedChange :: OrganizationChangeDTO
orgGlobalEditedChange =
  OrganizationChangeDTO
    {_name = orgGlobalEdited ^. name, _description = orgGlobalEdited ^. description, _email = orgGlobalEdited ^. email}

orgNetherlands :: Organization
orgNetherlands =
  Organization
    { _organizationId = "org.nl"
    , _name = "Organization Netherlands"
    , _description = "Some description of Organization Netherlands"
    , _email = "netherlands@example.com"
    , _role = UserRole
    , _token = "NetherlandsToken"
    , _active = True
    , _logo = Just orgLogo
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgStateDto :: OrganizationStateDTO
orgStateDto = OrganizationStateDTO {_active = True}
