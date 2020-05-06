module Wizard.Service.KnowledgeModel.Compilator.Modifier.Question where

import Control.Lens ((&), (.~), (^.))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import LensesExtension
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity Event Question where
  createEntity e@AddOptionsQuestionEvent {} =
    OptionsQuestion
      { _optionsQuestionUuid = e ^. entityUuid
      , _optionsQuestionTitle = e ^. title
      , _optionsQuestionText = e ^. text
      , _optionsQuestionRequiredLevel = e ^. requiredLevel
      , _optionsQuestionTagUuids = e ^. tagUuids
      , _optionsQuestionReferenceUuids = []
      , _optionsQuestionExpertUuids = []
      , _optionsQuestionAnswerUuids = []
      }
  createEntity e@AddListQuestionEvent {} =
    ListQuestion
      { _listQuestionUuid = e ^. entityUuid
      , _listQuestionTitle = e ^. title
      , _listQuestionText = e ^. text
      , _listQuestionRequiredLevel = e ^. requiredLevel
      , _listQuestionTagUuids = e ^. tagUuids
      , _listQuestionReferenceUuids = []
      , _listQuestionExpertUuids = []
      , _listQuestionItemTemplateQuestionUuids = []
      }
  createEntity e@AddValueQuestionEvent {} =
    ValueQuestion
      { _valueQuestionUuid = e ^. entityUuid
      , _valueQuestionTitle = e ^. title
      , _valueQuestionText = e ^. text
      , _valueQuestionRequiredLevel = e ^. requiredLevel
      , _valueQuestionTagUuids = e ^. tagUuids
      , _valueQuestionReferenceUuids = []
      , _valueQuestionExpertUuids = []
      , _valueQuestionValueType = e ^. valueType
      }
  createEntity e@AddIntegrationQuestionEvent {} =
    IntegrationQuestion
      { _integrationQuestionUuid = e ^. entityUuid
      , _integrationQuestionTitle = e ^. title
      , _integrationQuestionText = e ^. text
      , _integrationQuestionRequiredLevel = e ^. requiredLevel
      , _integrationQuestionTagUuids = e ^. tagUuids
      , _integrationQuestionReferenceUuids = []
      , _integrationQuestionExpertUuids = []
      , _integrationQuestionIntegrationUuid = e ^. integrationUuid
      , _integrationQuestionProps = e ^. props
      }

instance EditEntity Event Question where
  editEntity e' q =
    case e' of
      e@EditOptionsQuestionEvent {} -> applyToOptionsQuestion e . convertToOptionsQuestion $ q
      e@EditListQuestionEvent {} -> applyToListQuestion e . convertToListQuestion $ q
      e@EditValueQuestionEvent {} -> applyToValueQuestion e . convertToValueQuestion $ q
      e@EditIntegrationQuestionEvent {} -> applyToIntegrationQuestion e . convertToIntegrationQuestion $ q
    where
      applyToOptionsQuestion e =
        applyAnwerUuids e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToListQuestion e =
        applyItemTemplateQuestionUuids e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToValueQuestion e =
        applyValueType e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToIntegrationQuestion e =
        applyProps e .
        applyIntegrationUuid e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyTitle e q = applyValue (e ^. title) q title'
      applyText e q = applyValue (e ^. text) q text'
      applyRequiredLevel e q = applyValue (e ^. requiredLevel) q requiredLevel'
      applyTagUuids e q = applyValue (e ^. tagUuids) q tagUuids'
      applyExpertUuids e q = applyValue (e ^. expertUuids) q expertUuids'
      applyReferenceUuids e q = applyValue (e ^. referenceUuids) q referenceUuids'
      applyAnwerUuids e q = applyValue (e ^. answerUuids) q answerUuids'
      applyItemTemplateQuestionUuids e q = applyValue (e ^. itemTemplateQuestionUuids) q itemTemplateQuestionUuids'
      applyValueType e q = applyValue (e ^. valueType) q valueType'
      applyIntegrationUuid e q = applyValue (e ^. integrationUuid) q integrationUuid'
      applyProps e q = applyValue (e ^. props) q props'

convertToOptionsQuestion :: Question -> Question
convertToOptionsQuestion q@OptionsQuestion {} = OptionsQuestion' q
convertToOptionsQuestion q' =
  case q' of
    q@ListQuestion {} -> createQuestion q
    q@ValueQuestion {} -> createQuestion q
    q@IntegrationQuestion {} -> createQuestion q
  where
    createQuestion q =
      OptionsQuestion' $
      OptionsQuestion
        { _optionsQuestionUuid = q ^. uuid
        , _optionsQuestionTitle = q ^. title
        , _optionsQuestionText = q ^. text
        , _optionsQuestionRequiredLevel = q ^. requiredLevel
        , _optionsQuestionTagUuids = q ^. tagUuids
        , _optionsQuestionReferenceUuids = q ^. referenceUuids
        , _optionsQuestionExpertUuids = q ^. expertUuids
        , _optionsQuestionAnswerUuids = []
        }

convertToListQuestion :: Question -> Question
convertToListQuestion q@ListQuestion {} = ListQuestion' q
convertToListQuestion q' =
  case q' of
    q@OptionsQuestion {} -> createQuestion q
    q@ValueQuestion {} -> createQuestion q
    q@IntegrationQuestion {} -> createQuestion q
  where
    createQuestion q =
      ListQuestion' $
      ListQuestion
        { _listQuestionUuid = q ^. uuid
        , _listQuestionTitle = q ^. title
        , _listQuestionText = q ^. text
        , _listQuestionRequiredLevel = q ^. requiredLevel
        , _listQuestionTagUuids = q ^. tagUuids
        , _listQuestionReferenceUuids = q ^. referenceUuids
        , _listQuestionExpertUuids = q ^. expertUuids
        , _listQuestionItemTemplateQuestionUuids = []
        }

convertToValueQuestion :: Question -> Question
convertToValueQuestion q@ValueQuestion {} = ValueQuestion' q
convertToValueQuestion q' =
  case q' of
    q@OptionsQuestion {} -> createQuestion q
    q@ListQuestion {} -> createQuestion q
    q@IntegrationQuestion {} -> createQuestion q
  where
    createQuestion q =
      ValueQuestion' $
      ValueQuestion
        { _valueQuestionUuid = q ^. uuid
        , _valueQuestionTitle = q ^. title
        , _valueQuestionText = q ^. text
        , _valueQuestionRequiredLevel = q ^. requiredLevel
        , _valueQuestionTagUuids = q ^. tagUuids
        , _valueQuestionReferenceUuids = q ^. referenceUuids
        , _valueQuestionExpertUuids = q ^. expertUuids
        , _valueQuestionValueType = StringQuestionValueType
        }

convertToIntegrationQuestion :: Question -> Question
convertToIntegrationQuestion q@IntegrationQuestion {} = IntegrationQuestion' q
convertToIntegrationQuestion q' =
  case q' of
    q@OptionsQuestion {} -> createQuestion q
    q@ListQuestion {} -> createQuestion q
    q@ValueQuestion {} -> createQuestion q
  where
    createQuestion q =
      IntegrationQuestion' $
      IntegrationQuestion
        { _integrationQuestionUuid = q ^. uuid
        , _integrationQuestionTitle = q ^. title
        , _integrationQuestionText = q ^. text
        , _integrationQuestionRequiredLevel = q ^. requiredLevel
        , _integrationQuestionTagUuids = q ^. tagUuids
        , _integrationQuestionReferenceUuids = q ^. referenceUuids
        , _integrationQuestionExpertUuids = q ^. expertUuids
        , _integrationQuestionIntegrationUuid = U.nil
        , _integrationQuestionProps = M.empty
        }

updateIntegrationProps :: EditIntegrationEvent -> Question -> Question
updateIntegrationProps e q@IntegrationQuestion {} = IntegrationQuestion' $ q & props .~ updatedProps
  where
    updatedProps =
      if q ^. integrationUuid == e ^. entityUuid
        then case e ^. props of
               ChangedValue ps -> M.fromList . fmap (\p -> (p, fromMaybe "" (M.lookup p (q ^. props)))) $ ps
               NothingChanged -> q ^. props
        else q ^. props
updateIntegrationProps _ q' = q'

deleteIntegrationReference :: DeleteIntegrationEvent -> Question -> Question
deleteIntegrationReference e q@IntegrationQuestion {} =
  if q ^. integrationUuid == e ^. entityUuid
    then convertToValueQuestion . IntegrationQuestion' $ q
    else IntegrationQuestion' q
deleteIntegrationReference _ q' = q'

deleteTagReference :: DeleteTagEvent -> Question -> Question
deleteTagReference e q = q & tagUuids' .~ (L.delete (e ^. entityUuid) (q ^. tagUuids'))
