module LensesConfig where

import Control.Lens (makeFields, makeFieldsNoPrefix)

import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / Event
makeFields ''EventField

makeFields ''Event

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

-- Model / Package
makeFieldsNoPrefix ''Package

makeFieldsNoPrefix ''PackageWithEvents

-- Model / PackageBundle
makeFieldsNoPrefix ''PackageBundle

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / Package
makeFieldsNoPrefix ''PackageDTO

-- Api / Resource / PackageBundle
makeFieldsNoPrefix ''PackageBundleDTO
