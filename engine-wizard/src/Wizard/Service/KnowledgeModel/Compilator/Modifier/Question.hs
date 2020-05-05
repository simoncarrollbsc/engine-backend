module Wizard.Service.KnowledgeModel.Compilator.Modifier.Question where

import Control.Lens ((&), (.~), (^.))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddQuestionEvent Question where
  createEntity (AddOptionsQuestionEvent' e) =
    OptionsQuestion' $
    OptionsQuestion
      { _uuid = e ^. entityUuid
      , _title = e ^. title
      , _text = e ^. text
      , _requiredLevel = e ^. requiredLevel
      , _tagUuids = e ^. tagUuids
      , _referenceUuids = []
      , _uuids = []
      , _uuids = []
      }
  createEntity (AddListQuestionEvent' e) =
    ListQuestion' $
    ListQuestion
      { _uuid = e ^. entityUuid
      , _title = e ^. title
      , _text = e ^. text
      , _requiredLevel = e ^. requiredLevel
      , _tagUuids = e ^. tagUuids
      , _referenceUuids = []
      , _uuids = []
      , _itemTemplateQuestionUuids = []
      }
  createEntity (AddValueQuestionEvent' e) =
    ValueQuestion' $
    ValueQuestion
      { _uuid = e ^. entityUuid
      , _title = e ^. title
      , _text = e ^. text
      , _requiredLevel = e ^. requiredLevel
      , _tagUuids = e ^. tagUuids
      , _referenceUuids = []
      , _uuids = []
      , _valueType = e ^. valueType
      }
  createEntity (AddIntegrationQuestionEvent' e) =
    IntegrationQuestion' $
    IntegrationQuestion
      { _uuid = e ^. entityUuid
      , _title = e ^. title
      , _text = e ^. text
      , _requiredLevel = e ^. requiredLevel
      , _tagUuids = e ^. tagUuids
      , _referenceUuids = []
      , _uuids = []
      , _uuid = e ^. integrationUuid
      , _props = e ^. props
      }

instance EditEntity EditQuestionEvent Question where
  editEntity e' q =
    case e' of
      (EditOptionsQuestionEvent' e) -> applyToOptionsQuestion e . convertToOptionsQuestion $ q
      (EditListQuestionEvent' e) -> applyToListQuestion e . convertToListQuestion $ q
      (EditValueQuestionEvent' e) -> applyToValueQuestion e . convertToValueQuestion $ q
      (EditIntegrationQuestionEvent' e) -> applyToIntegrationQuestion e . convertToIntegrationQuestion $ q
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
convertToOptionsQuestion (OptionsQuestion' q) = OptionsQuestion' q
convertToOptionsQuestion q' =
  case q' of
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      OptionsQuestion' $
      OptionsQuestion
        { _uuid = q ^. uuid
        , _title = q ^. title
        , _text = q ^. text
        , _requiredLevel = q ^. requiredLevel
        , _tagUuids = q ^. tagUuids
        , _referenceUuids = q ^. referenceUuids
        , _uuids = q ^. expertUuids
        , _uuids = []
        }

convertToListQuestion :: Question -> Question
convertToListQuestion (ListQuestion' q) = ListQuestion' q
convertToListQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ListQuestion' $
      ListQuestion
        { _uuid = q ^. uuid
        , _title = q ^. title
        , _text = q ^. text
        , _requiredLevel = q ^. requiredLevel
        , _tagUuids = q ^. tagUuids
        , _referenceUuids = q ^. referenceUuids
        , _uuids = q ^. expertUuids
        , _itemTemplateQuestionUuids = []
        }

convertToValueQuestion :: Question -> Question
convertToValueQuestion (ValueQuestion' q) = ValueQuestion' q
convertToValueQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ValueQuestion' $
      ValueQuestion
        { _uuid = q ^. uuid
        , _title = q ^. title
        , _text = q ^. text
        , _requiredLevel = q ^. requiredLevel
        , _tagUuids = q ^. tagUuids
        , _referenceUuids = q ^. referenceUuids
        , _uuids = q ^. expertUuids
        , _valueType = StringQuestionValueType
        }

convertToIntegrationQuestion :: Question -> Question
convertToIntegrationQuestion (IntegrationQuestion' q) = IntegrationQuestion' q
convertToIntegrationQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
  where
    createQuestion q =
      IntegrationQuestion' $
      IntegrationQuestion
        { _uuid = q ^. uuid
        , _title = q ^. title
        , _text = q ^. text
        , _requiredLevel = q ^. requiredLevel
        , _tagUuids = q ^. tagUuids
        , _referenceUuids = q ^. referenceUuids
        , _uuids = q ^. expertUuids
        , _uuid = U.nil
        , _props = M.empty
        }

updateIntegrationProps :: EditIntegrationEvent -> Question -> Question
updateIntegrationProps e (IntegrationQuestion' q) = IntegrationQuestion' $ q & props .~ updatedProps
  where
    updatedProps =
      if q ^. integrationUuid == e ^. entityUuid
        then case e ^. props of
               ChangedValue ps -> M.fromList . fmap (\p -> (p, fromMaybe "" (M.lookup p (q ^. props)))) $ ps
               NothingChanged -> q ^. props
        else q ^. props
updateIntegrationProps _ q' = q'

deleteIntegrationReference :: DeleteIntegrationEvent -> Question -> Question
deleteIntegrationReference e (IntegrationQuestion' q) =
  if q ^. integrationUuid == e ^. entityUuid
    then convertToValueQuestion . IntegrationQuestion' $ q
    else IntegrationQuestion' q
deleteIntegrationReference _ q' = q'

deleteTagReference :: DeleteTagEvent -> Question -> Question
deleteTagReference e q = q & tagUuids' .~ (L.delete (e ^. entityUuid) (q ^. tagUuids'))
