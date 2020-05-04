module LensesConfig where

import Control.Lens (makeFields, makeFieldsNoPrefix)

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Model.Statistics.InstanceStatistics
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFieldsNoPrefix ''ActionKey

-- Model / Config
makeFieldsNoPrefix ''ServerConfig

makeFieldsNoPrefix ''ServerConfigGeneral

makeFieldsNoPrefix ''ServerConfigDatabase

makeFieldsNoPrefix ''ServerConfigMail

makeFieldsNoPrefix ''ServerConfigAnalytics

makeFieldsNoPrefix ''BuildInfoConfig

-- Model / Context
makeFieldsNoPrefix ''BaseContext

makeFieldsNoPrefix ''AppContext

-- Model / Event
makeFields ''Event

makeFields ''EventField

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''KnowledgeModelEntities

makeFields ''Chapter

makeFields ''Question

makeFields ''OptionsQuestion

makeFields ''ListQuestion

makeFields ''ValueQuestion

makeFields ''IntegrationQuestion

makeFields ''Answer

makeFields ''Expert

makeFields ''Reference

makeFields ''ResourcePageReference

makeFields ''URLReference

makeFields ''CrossReference

makeFields ''Metric

makeFields ''MetricMeasure

makeFields ''Tag

makeFields ''Integration

-- Model / Organization
makeFieldsNoPrefix ''Organization

-- Model / Package
makeFieldsNoPrefix ''Package

makeFieldsNoPrefix ''PackageWithEvents

-- Model / PackageBundle
makeFieldsNoPrefix ''PackageBundle

-- Model / Statistics
makeFieldsNoPrefix ''InstanceStatistics

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKey
makeFieldsNoPrefix ''ActionKeyDTO

-- Api / Resource / Info
makeFieldsNoPrefix ''InfoDTO

-- Api / Resource / Organization
makeFieldsNoPrefix ''OrganizationDTO

makeFieldsNoPrefix ''OrganizationCreateDTO

makeFieldsNoPrefix ''OrganizationChangeDTO

makeFieldsNoPrefix ''OrganizationStateDTO

-- Api / Resource / Package
makeFieldsNoPrefix ''PackageDTO

makeFieldsNoPrefix ''PackageSimpleDTO

makeFieldsNoPrefix ''PackageDetailDTO

-- Api / Resource / PackageBundle
makeFieldsNoPrefix ''PackageBundleDTO
