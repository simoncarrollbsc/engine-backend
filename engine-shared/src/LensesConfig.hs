module LensesConfig where

import Control.Lens (makeFields)

import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Common.Pageable
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.AppContext
import Shared.Model.Context.BaseContext
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageGroup
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / Common
makeFields ''Pageable

-- Model / Config
makeFields ''ServerConfigDatabase

makeFields ''ServerConfigMail

makeFields ''ServerConfigAnalytics

makeFields ''ServerConfigLogging

-- Model / Context
makeFields ''AppContext

makeFields ''BaseContext

-- Model / Event
makeFields ''EventField

makeFields ''AddKnowledgeModelEvent

makeFields ''EditKnowledgeModelEvent

makeFields ''AddChapterEvent

makeFields ''EditChapterEvent

makeFields ''DeleteChapterEvent

makeFields ''AddQuestionEvent

makeFields ''AddOptionsQuestionEvent

makeFields ''AddMultiChoiceQuestionEvent

makeFields ''AddListQuestionEvent

makeFields ''AddValueQuestionEvent

makeFields ''AddIntegrationQuestionEvent

makeFields ''EditQuestionEvent

makeFields ''EditOptionsQuestionEvent

makeFields ''EditMultiChoiceQuestionEvent

makeFields ''EditListQuestionEvent

makeFields ''EditValueQuestionEvent

makeFields ''EditIntegrationQuestionEvent

makeFields ''DeleteQuestionEvent

makeFields ''AddAnswerEvent

makeFields ''EditAnswerEvent

makeFields ''DeleteAnswerEvent

makeFields ''AddChoiceEvent

makeFields ''EditChoiceEvent

makeFields ''DeleteChoiceEvent

makeFields ''AddExpertEvent

makeFields ''EditExpertEvent

makeFields ''DeleteExpertEvent

makeFields ''AddReferenceEvent

makeFields ''AddResourcePageReferenceEvent

makeFields ''AddURLReferenceEvent

makeFields ''AddCrossReferenceEvent

makeFields ''EditReferenceEvent

makeFields ''EditResourcePageReferenceEvent

makeFields ''EditURLReferenceEvent

makeFields ''EditCrossReferenceEvent

makeFields ''DeleteReferenceEvent

makeFields ''AddTagEvent

makeFields ''EditTagEvent

makeFields ''DeleteTagEvent

makeFields ''AddIntegrationEvent

makeFields ''EditIntegrationEvent

makeFields ''DeleteIntegrationEvent

makeFields ''MoveQuestionEvent

makeFields ''MoveAnswerEvent

makeFields ''MoveChoiceEvent

makeFields ''MoveExpertEvent

makeFields ''MoveReferenceEvent

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''KnowledgeModelEntities

makeFields ''Chapter

makeFields ''Question

makeFields ''OptionsQuestion

makeFields ''MultiChoiceQuestion

makeFields ''ListQuestion

makeFields ''ValueQuestion

makeFields ''IntegrationQuestion

makeFields ''Answer

makeFields ''Choice

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
makeFields ''Package

makeFields ''PackageWithEvents

makeFields ''PackageGroup

-- Model / PackageBundle
makeFields ''PackageBundle

-- Model / Template
makeFields ''Template

makeFields ''TemplateAllowedPackage

makeFields ''TemplateFormat

makeFields ''TemplateFile

makeFields ''TemplateAsset

makeFields ''TemplateGroup

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / Package
makeFields ''PackageDTO

-- Api / Resource / PackageBundle
makeFields ''PackageBundleDTO
