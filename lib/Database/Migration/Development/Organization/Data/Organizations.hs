module Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Organization.OrganizationChangeDTO
import LensesConfig
import Model.Organization.Organization

org1 =
  Organization
  { _organizationUuid = fromJust $ U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"
  , _organizationName = "Elixir Amsterdam"
  , _organizationOrganizationId = "elixir.nl.amsterdam"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

editedOrg1 =
  Organization
  { _organizationUuid = org1 ^. uuid
  , _organizationName = "EDITED: Elixir Leiden"
  , _organizationOrganizationId = "elixir.nl.leiden"
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
  }

editedOrg1Change =
  OrganizationChangeDTO
  { _organizationChangeDTOUuid = editedOrg1 ^. uuid
  , _organizationChangeDTOName = editedOrg1 ^. name
  , _organizationChangeDTOOrganizationId = editedOrg1 ^. organizationId
  }
