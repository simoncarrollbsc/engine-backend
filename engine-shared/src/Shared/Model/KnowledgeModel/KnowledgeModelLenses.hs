module Shared.Model.KnowledgeModel.KnowledgeModelLenses where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.LensesClasses

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
chaptersL :: Functor f => ([Chapter] -> f [Chapter]) -> KnowledgeModel -> f KnowledgeModel
chaptersL = createEntityLFn (entities . chapters)

chaptersM :: Functor f => ((M.Map U.UUID Chapter) -> f (M.Map U.UUID Chapter)) -> KnowledgeModel -> f KnowledgeModel
chaptersM = createEntityMFn (entities . chapters)

------------------------------------------------------------------------------------------
questionsL :: Functor f => ([Question] -> f [Question]) -> KnowledgeModel -> f KnowledgeModel
questionsL = createEntityLFn (entities . questions)

questionsM :: Functor f => ((M.Map U.UUID Question) -> f (M.Map U.UUID Question)) -> KnowledgeModel -> f KnowledgeModel
questionsM = createEntityMFn (entities . questions)

------------------------------------------------------------------------------------------
answersL :: Functor f => ([Answer] -> f [Answer]) -> KnowledgeModel -> f KnowledgeModel
answersL = createEntityLFn (entities . answers)

answersM :: Functor f => ((M.Map U.UUID Answer) -> f (M.Map U.UUID Answer)) -> KnowledgeModel -> f KnowledgeModel
answersM = createEntityMFn (entities . answers)

------------------------------------------------------------------------------------------
expertsL :: Functor f => ([Expert] -> f [Expert]) -> KnowledgeModel -> f KnowledgeModel
expertsL = createEntityLFn (entities . experts)

expertsM :: Functor f => ((M.Map U.UUID Expert) -> f (M.Map U.UUID Expert)) -> KnowledgeModel -> f KnowledgeModel
expertsM = createEntityMFn (entities . experts)

------------------------------------------------------------------------------------------
referencesL :: Functor f => ([Reference] -> f [Reference]) -> KnowledgeModel -> f KnowledgeModel
referencesL = createEntityLFn (entities . references)

referencesM ::
     Functor f => ((M.Map U.UUID Reference) -> f (M.Map U.UUID Reference)) -> KnowledgeModel -> f KnowledgeModel
referencesM = createEntityMFn (entities . references)

------------------------------------------------------------------------------------------
integrationsL :: Functor f => ([Integration] -> f [Integration]) -> KnowledgeModel -> f KnowledgeModel
integrationsL = createEntityLFn (entities . integrations)

integrationsM ::
     Functor f => ((M.Map U.UUID Integration) -> f (M.Map U.UUID Integration)) -> KnowledgeModel -> f KnowledgeModel
integrationsM = createEntityMFn (entities . integrations)

------------------------------------------------------------------------------------------
tagsL :: Functor f => ([Tag] -> f [Tag]) -> KnowledgeModel -> f KnowledgeModel
tagsL = createEntityLFn (entities . tags)

tagsM :: Functor f => ((M.Map U.UUID Tag) -> f (M.Map U.UUID Tag)) -> KnowledgeModel -> f KnowledgeModel
tagsM = createEntityMFn (entities . tags)

------------------------------------------------------------------------------------------
createEntityLFn ::
     (HasUuid' a U.UUID, Functor f)
  => (Lens' KnowledgeModel (M.Map U.UUID a))
  -> ([a] -> f [a])
  -> KnowledgeModel
  -> f KnowledgeModel
createEntityLFn accessor convert km = fmap (update km) (convert . M.elems $ km ^. accessor)
  where
    update km newValue = km & accessor .~ (toMap newValue)

createEntityMFn ::
     Functor f
  => (Lens' KnowledgeModel (M.Map U.UUID a))
  -> (M.Map U.UUID a -> f (M.Map U.UUID a))
  -> KnowledgeModel
  -> f KnowledgeModel
createEntityMFn accessor convert km = fmap (update km) (convert $ km ^. accessor)
  where
    update km newValue = km & accessor .~ newValue

toMap :: HasUuid' a U.UUID => [a] -> M.Map U.UUID a
toMap = M.fromList . fmap (\entity -> (entity ^. uuid', entity))

-- -------------------------
-- UUID --------------------
-- -------------------------
instance HasUuid' KnowledgeModel U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: KnowledgeModel -> U.UUID
      get entity = entity ^. uuid
      set :: KnowledgeModel -> U.UUID -> KnowledgeModel
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Chapter U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Chapter -> U.UUID
      get entity = entity ^. uuid
      set :: Chapter -> U.UUID -> Chapter
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Question U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> U.UUID
      get q@ListQuestion {} = _listQuestionUuid q
      get q@OptionsQuestion {} = _optionsQuestionUuid q
      get q@ValueQuestion {} = _valueQuestionUuid q
      get q@IntegrationQuestion {} = _integrationQuestionUuid q
      set :: Question -> U.UUID -> Question
      set q@ListQuestion {} newValue = q {_listQuestionUuid = newValue}
      set q@OptionsQuestion {} newValue = q {_optionsQuestionUuid = newValue}
      set q@ValueQuestion {} newValue = q {_valueQuestionUuid = newValue}
      set q@IntegrationQuestion {} newValue = q {_integrationQuestionUuid = newValue}

instance HasUuid' Expert U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Expert -> U.UUID
      get entity = entity ^. uuid
      set :: Expert -> U.UUID -> Expert
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Reference U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> U.UUID
      get entity@ResourcePageReference {} = _resourcePageReferenceUuid entity
      get entity@URLReference {} = _uRLReferenceUuid entity
      get entity@CrossReference {} = _crossReferenceUuid entity
      set :: Reference -> U.UUID -> Reference
      set entity@ResourcePageReference {} newValue = entity {_resourcePageReferenceUuid = newValue}
      set entity@URLReference {} newValue = entity {_uRLReferenceUuid = newValue}
      set entity@CrossReference {} newValue = entity {_crossReferenceUuid = newValue}

instance HasUuid' Answer U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Answer -> U.UUID
      get entity = entity ^. uuid
      set :: Answer -> U.UUID -> Answer
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Tag U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Tag -> U.UUID
      get entity = entity ^. uuid
      set :: Tag -> U.UUID -> Tag
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Integration U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Integration -> U.UUID
      get entity = entity ^. uuid
      set :: Integration -> U.UUID -> Integration
      set entity newValue = entity & uuid .~ newValue

-- -------------------------
-- QUESTION ----------------
-- -------------------------
title' :: Functor f => (String -> f String) -> Question -> f Question
title' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> String
    get q@ListQuestion {} = _listQuestionTitle q
    get q@OptionsQuestion {} = _optionsQuestionTitle q
    get q@ValueQuestion {} = _valueQuestionTitle q
    get q@IntegrationQuestion {} = _integrationQuestionTitle q
    set :: Question -> String -> Question
    set q@ListQuestion {} newValue = q {_listQuestionTitle = newValue}
    set q@OptionsQuestion {} newValue = q {_optionsQuestionTitle = newValue}
    set q@ValueQuestion {} newValue = q {_valueQuestionTitle = newValue}
    set q@IntegrationQuestion {} newValue = q {_integrationQuestionTitle = newValue}

------------------------------------------------------------------------------------------
text' :: Functor f => (Maybe String -> f (Maybe String)) -> Question -> f Question
text' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe String
    get q@ListQuestion {} = _listQuestionText q
    get q@OptionsQuestion {} = _optionsQuestionText q
    get q@ValueQuestion {} = _valueQuestionText q
    get q@IntegrationQuestion {} = _integrationQuestionText q
    set :: Question -> Maybe String -> Question
    set q@ListQuestion {} newValue = q {_listQuestionText = newValue}
    set q@OptionsQuestion {} newValue = q {_optionsQuestionText = newValue}
    set q@ValueQuestion {} newValue = q {_valueQuestionText = newValue}
    set q@IntegrationQuestion {} newValue = q {_integrationQuestionText = newValue}

------------------------------------------------------------------------------------------
requiredLevel' :: Functor f => (Maybe Int -> f (Maybe Int)) -> Question -> f Question
requiredLevel' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe Int
    get q@ListQuestion {} = _listQuestionRequiredLevel q
    get q@OptionsQuestion {} = _optionsQuestionRequiredLevel q
    get q@ValueQuestion {} = _valueQuestionRequiredLevel q
    get q@IntegrationQuestion {} = _integrationQuestionRequiredLevel q
    set :: Question -> Maybe Int -> Question
    set q@ListQuestion {} newValue = q {_listQuestionRequiredLevel = newValue}
    set q@OptionsQuestion {} newValue = q {_optionsQuestionRequiredLevel = newValue}
    set q@ValueQuestion {} newValue = q {_valueQuestionRequiredLevel = newValue}
    set q@IntegrationQuestion {} newValue = q {_integrationQuestionRequiredLevel = newValue}

------------------------------------------------------------------------------------------
instance HasTagUuids' Question [U.UUID] where
  tagUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get q@ListQuestion {} = _listQuestionTagUuids q
      get q@OptionsQuestion {} = _optionsQuestionTagUuids q
      get q@ValueQuestion {} = _valueQuestionTagUuids q
      get q@IntegrationQuestion {} = _integrationQuestionTagUuids q
      set :: Question -> [U.UUID] -> Question
      set q@ListQuestion {} newValue = q {_listQuestionTagUuids = newValue}
      set q@OptionsQuestion {} newValue = q {_optionsQuestionTagUuids = newValue}
      set q@ValueQuestion {} newValue = q {_valueQuestionTagUuids = newValue}
      set q@IntegrationQuestion {} newValue = q {_integrationQuestionTagUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasExpertUuids' Question [U.UUID] where
  expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
        get :: Question -> [U.UUID]
        get q@ListQuestion {} = _listQuestionExpertUuids q
        get q@OptionsQuestion {} = _optionsQuestionExpertUuids q
        get q@ValueQuestion {} = _valueQuestionExpertUuids q
        get q@IntegrationQuestion {} = _integrationQuestionExpertUuids q
        set :: Question -> [U.UUID] -> Question
        set q@ListQuestion {} newValue = q {_listQuestionExpertUuids = newValue}
        set q@OptionsQuestion {} newValue = q {_optionsQuestionExpertUuids = newValue}
        set q@ValueQuestion {} newValue = q {_valueQuestionExpertUuids = newValue}
        set q@IntegrationQuestion {} newValue = q {_integrationQuestionExpertUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasReferenceUuids' Question [U.UUID] where
  referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get q@ListQuestion {} = _listQuestionReferenceUuids q
      get q@OptionsQuestion {} = _optionsQuestionReferenceUuids q
      get q@ValueQuestion {} = _valueQuestionReferenceUuids q
      get q@IntegrationQuestion {} = _integrationQuestionReferenceUuids q
      set :: Question -> [U.UUID] -> Question
      set q@ListQuestion {} newValue = q {_listQuestionReferenceUuids = newValue}
      set q@OptionsQuestion {} newValue = q {_optionsQuestionReferenceUuids = newValue}
      set q@ValueQuestion {} newValue = q {_valueQuestionReferenceUuids = newValue}
      set q@IntegrationQuestion {} newValue = q {_integrationQuestionReferenceUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasAnswerUuids' Question [U.UUID] where
  answerUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get q@OptionsQuestion {} = _optionsQuestionAnswerUuids q
      get q = []
      set :: Question -> [U.UUID] -> Question
      set q@OptionsQuestion {} newValue = q {_optionsQuestionAnswerUuids = newValue}
      set q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasItemTemplateQuestionUuids' Question [U.UUID] where
  itemTemplateQuestionUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get q@ListQuestion {} = _listQuestionItemTemplateQuestionUuids q
      get q = []
      set :: Question -> [U.UUID] -> Question
      set q@ListQuestion {} newValue = q {_listQuestionItemTemplateQuestionUuids = newValue}
      set q newValue = q

-- ------------------------------------------------------------------------------------------
valueType' :: Functor f => (QuestionValueType -> f QuestionValueType) -> Question -> f Question
valueType' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> QuestionValueType
    get q@ValueQuestion {} = _valueQuestionValueType q
    get q = StringQuestionValueType
    set :: Question -> QuestionValueType -> Question
    set q@ValueQuestion {} newValue = q {_valueQuestionValueType = newValue}
    set q newValue = q

-- ------------------------------------------------------------------------------------------
integrationUuid' :: Functor f => (U.UUID -> f U.UUID) -> Question -> f Question
integrationUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> U.UUID
    get q@IntegrationQuestion {} = _integrationQuestionIntegrationUuid q
    get q = U.nil
    set :: Question -> U.UUID -> Question
    set q@IntegrationQuestion {} newValue = q {_integrationQuestionIntegrationUuid = newValue}
    set q newValue = q

-- ------------------------------------------------------------------------------------------
props' :: Functor f => (M.Map String String -> f (M.Map String String)) -> Question -> f Question
props' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> M.Map String String
    get q@IntegrationQuestion {} = _integrationQuestionProps q
    get q = M.empty
    set :: Question -> M.Map String String -> Question
    set q@IntegrationQuestion {} newValue = q {_integrationQuestionProps = newValue}
    set q newValue = q

-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance HasShortUuid' Reference String where
  shortUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> String
      get ref@ResourcePageReference {} = _resourcePageReferenceShortUuid ref
      get ref = ""
      set :: Reference -> String -> Reference
      set ref@ResourcePageReference {} newValue = ref {_resourcePageReferenceShortUuid = newValue}
      set ref newValue = ref

-- ------------------------------------------------------------------------------------------
instance HasUrl' Reference String where
  url' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> String
      get ref@URLReference {} = _uRLReferenceUrl ref
      get ref = ""
      set :: Reference -> String -> Reference
      set ref@URLReference {} newValue = ref {_uRLReferenceUrl = newValue}
      set ref newValue = ref

-- ------------------------------------------------------------------------------------------
instance HasLabel' Reference String where
  label' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> String
      get ref@URLReference {} = _uRLReferenceLabel ref
      get ref = ""
      set :: Reference -> String -> Reference
      set ref@URLReference {} newValue = ref {_uRLReferenceLabel = newValue}
      set ref newValue = ref

-- ------------------------------------------------------------------------------------------
instance HasTargetUuid' Reference U.UUID where
  targetUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> U.UUID
      get ref@CrossReference {} = _crossReferenceTargetUuid ref
      get ref = U.nil
      set :: Reference -> U.UUID -> Reference
      set ref@CrossReference {} newValue = ref {_crossReferenceTargetUuid = newValue}
      set ref newValue = ref

-- ------------------------------------------------------------------------------------------
instance HasDescription' Reference String where
  description' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> String
      get ref@CrossReference {} = _crossReferenceDescription ref
      get ref = ""
      set :: Reference -> String -> Reference
      set ref@CrossReference {} newValue = ref {_crossReferenceDescription = newValue}
      set ref newValue = ref