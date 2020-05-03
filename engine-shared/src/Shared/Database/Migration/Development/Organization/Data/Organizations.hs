module Shared.Database.Migration.Development.Organization.Data.Organizations where

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

orgGlobalSimple :: OrganizationSimpleDTO
orgGlobalSimple = OrganizationSimpleDTO {_name = "Organization", _organizationId = "global", _logo = Just orgLogo}

orgNetherlandsSimple :: OrganizationSimpleDTO
orgNetherlandsSimple =
  OrganizationSimpleDTO {_name = "Organization Netherlands", _organizationId = "org.nl", _logo = Just orgLogo}

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
