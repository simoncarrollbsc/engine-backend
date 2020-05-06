module LensesConfig where

import Control.Lens (makeFields)

import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Event.EventField
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / Event
makeFields ''Event

makeFields ''EventField

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''KnowledgeModelEntities

makeFields ''Chapter

makeFields ''Question

makeFields ''Answer

makeFields ''Expert

makeFields ''Reference

makeFields ''Metric

makeFields ''MetricMeasure

makeFields ''Tag

makeFields ''Integration

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

-- Model / PackageBundle
makeFields ''PackageBundle

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / Package
makeFields ''PackageDTO

-- Api / Resource / PackageBundle
makeFields ''PackageBundleDTO
