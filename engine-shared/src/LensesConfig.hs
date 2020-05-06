module LensesConfig where

import Control.Lens (makeFieldsNoPrefix)

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
--makeFieldsNoPrefix ''Event

makeFieldsNoPrefix ''EventField

-- Model / KnowledgeModel
makeFieldsNoPrefix ''KnowledgeModel

makeFieldsNoPrefix ''KnowledgeModelEntities

makeFieldsNoPrefix ''Chapter

makeFieldsNoPrefix ''Question

makeFieldsNoPrefix ''Answer

makeFieldsNoPrefix ''Expert

makeFieldsNoPrefix ''Reference

makeFieldsNoPrefix ''Metric

makeFieldsNoPrefix ''MetricMeasure

makeFieldsNoPrefix ''Tag

makeFieldsNoPrefix ''Integration

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
