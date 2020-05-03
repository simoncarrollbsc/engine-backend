module Wizard.Database.Migration.Development.Registry.Data.Registries where

import Control.Lens ((^.))

import LensesConfig
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO

registryCreate :: RegistryCreateDTO
registryCreate = RegistryCreateDTO {_email = "albert.einstein@example.com"}

registryConfirmation :: RegistryConfirmationDTO
registryConfirmation =
  RegistryConfirmationDTO {_organizationId = regActionKey ^. organizationId, _hash = regActionKey ^. hash}
