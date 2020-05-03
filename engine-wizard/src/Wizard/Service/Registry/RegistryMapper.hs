module Wizard.Service.Registry.RegistryMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Config.AppConfig

toOrganizationCreate :: AppConfig -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate appConfig reqDto =
  OrganizationCreateDTO
    { _organizationId = appConfig ^. organization . organizationId
    , _name = appConfig ^. organization . name
    , _description = appConfig ^. organization . description
    , _email = reqDto ^. email
    }
