module Shared.Model.KnowledgeModel.KnowledgeModelLenses where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

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
     (HasUuid' a, Functor f)
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

toMap :: HasUuid' a => [a] -> M.Map U.UUID a
toMap = M.fromList . fmap (\entity -> (entity ^. uuid', entity))

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
class HasUuid' entity where
  uuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

instance HasUuid' KnowledgeModel where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: KnowledgeModel -> U.UUID
      get entity = entity ^. uuid
      set :: KnowledgeModel -> U.UUID -> KnowledgeModel
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Chapter where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Chapter -> U.UUID
      get entity = entity ^. uuid
      set :: Chapter -> U.UUID -> Chapter
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Question where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> U.UUID
      get entity@ListQuestion {} = entity ^. uuid
      get entity@OptionsQuestion {} = entity ^. uuid
      get entity@ValueQuestion {} = entity ^. uuid
      get entity@IntegrationQuestion {} = entity ^. uuid
      set :: Question -> U.UUID -> Question
      set entity@ListQuestion {} newValue = entity & uuid .~ newValue
      set entity@OptionsQuestion {} newValue = entity & uuid .~ newValue
      set entity@ValueQuestion {} newValue = entity & uuid .~ newValue
      set entity@IntegrationQuestion {} newValue = entity & uuid .~ newValue

instance HasUuid' Expert where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Expert -> U.UUID
      get entity = entity ^. uuid
      set :: Expert -> U.UUID -> Expert
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Reference where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Reference -> U.UUID
      get entity@ResourcePageReference {} = entity ^. uuid
      get entity@URLReference {} = entity ^. uuid
      get entity@CrossReference {} = entity ^. uuid
      set :: Reference -> U.UUID -> Reference
      set entity@ResourcePageReference {} newValue =  entity & uuid .~ newValue
      set entity@URLReference {} newValue =  entity & uuid .~ newValue
      set entity@CrossReference {} newValue =  entity & uuid .~ newValue

instance HasUuid' Answer where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Answer -> U.UUID
      get entity = entity ^. uuid
      set :: Answer -> U.UUID -> Answer
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Tag where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Tag -> U.UUID
      get entity = entity ^. uuid
      set :: Tag -> U.UUID -> Tag
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Integration where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Integration -> U.UUID
      get entity = entity ^. uuid
      set :: Integration -> U.UUID -> Integration
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
title' :: Functor f => (String -> f String) -> Question -> f Question
title' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> String
    get q@ListQuestion {} = q ^. title
    get q@OptionsQuestion {} = q ^. title
    get q@ValueQuestion {} = q ^. title
    get q@IntegrationQuestion {} = q ^. title
    set :: Question -> String -> Question
    set q@ListQuestion {} newValue = q & title .~ newValue
    set q@OptionsQuestion {} newValue = q & title .~ newValue
    set q@ValueQuestion {} newValue = q & title .~ newValue
    set q@IntegrationQuestion {} newValue = q & title .~ newValue

------------------------------------------------------------------------------------------
text' :: Functor f => (Maybe String -> f (Maybe String)) -> Question -> f Question
text' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe String
    get q@ListQuestion {} = q ^. text
    get q@OptionsQuestion {} = q ^. text
    get q@ValueQuestion {} = q ^. text
    get q@IntegrationQuestion {} = q ^. text
    set :: Question -> Maybe String -> Question
    set q@ListQuestion {} newValue = q & text .~ newValue
    set q@OptionsQuestion {} newValue = q & text .~ newValue
    set q@ValueQuestion {} newValue = q & text .~ newValue
    set q@IntegrationQuestion {} newValue = q & text .~ newValue

------------------------------------------------------------------------------------------
requiredLevel' :: Functor f => (Maybe Int -> f (Maybe Int)) -> Question -> f Question
requiredLevel' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe Int
    get q@ListQuestion {} = _requiredLevel q
    get q@OptionsQuestion {} = _requiredLevel q
    get q@ValueQuestion {} = _requiredLevel q
    get q@IntegrationQuestion {} = _requiredLevel q
    set :: Question -> Maybe Int -> Question
    set q@ListQuestion {} newValue = q { _requiredLevel = newValue }
    set q@OptionsQuestion {} newValue = q { _requiredLevel = newValue }
    set q@ValueQuestion {} newValue = q { _requiredLevel = newValue }
    set q@IntegrationQuestion {} newValue = q { _requiredLevel = newValue }

------------------------------------------------------------------------------------------
tagUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
tagUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get q@ListQuestion {} = q ^. tagUuids
    get q@OptionsQuestion {} = q ^. tagUuids
    get q@ValueQuestion {} = q ^. tagUuids
    get q@IntegrationQuestion {} = q ^. tagUuids
    set :: Question -> [U.UUID] -> Question
    set q@ListQuestion {} newValue = q & tagUuids .~ newValue
    set q@OptionsQuestion {} newValue = q & tagUuids .~ newValue
    set q@ValueQuestion {} newValue = q & tagUuids .~ newValue
    set q@IntegrationQuestion {} newValue = q & tagUuids .~ newValue

-- ------------------------------------------------------------------------------------------
expertUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get q@ListQuestion {} = q ^. expertUuids
    get q@OptionsQuestion {} = q ^. expertUuids
    get q@ValueQuestion {} = q ^. expertUuids
    get q@IntegrationQuestion {} = q ^. expertUuids
    set :: Question -> [U.UUID] -> Question
    set q@ListQuestion {} newValue = q & expertUuids .~ newValue
    set q@OptionsQuestion {} newValue = q & expertUuids .~ newValue
    set q@ValueQuestion {} newValue = q & expertUuids .~ newValue
    set q@IntegrationQuestion {} newValue = q & expertUuids .~ newValue

-- ------------------------------------------------------------------------------------------
referenceUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get q@ListQuestion {} = q ^. referenceUuids
    get q@OptionsQuestion {} = q ^. referenceUuids
    get q@ValueQuestion {} = q ^. referenceUuids
    get q@IntegrationQuestion {} = q ^. referenceUuids
    set :: Question -> [U.UUID] -> Question
    set q@ListQuestion {} newValue = q & referenceUuids .~ newValue
    set q@OptionsQuestion {} newValue = q & referenceUuids .~ newValue
    set q@ValueQuestion {} newValue = q & referenceUuids .~ newValue
    set q@IntegrationQuestion {} newValue = q & referenceUuids .~ newValue

-- ------------------------------------------------------------------------------------------
answerUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
answerUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get q@OptionsQuestion {} = q ^. answerUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set q@OptionsQuestion {} newValue = q & answerUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
itemTemplateQuestionUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
itemTemplateQuestionUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get q@ListQuestion {} = q ^. itemTemplateQuestionUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set q@ListQuestion {} newValue = q & itemTemplateQuestionUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
valueType' :: Functor f => (QuestionValueType -> f QuestionValueType) -> Question -> f Question
valueType' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> QuestionValueType
    get q@ValueQuestion {} = _valueType q
    get q = StringQuestionValueType
    set :: Question -> QuestionValueType -> Question
    set q@ValueQuestion {} newValue = q & valueType .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
integrationUuid' :: Functor f => (U.UUID -> f U.UUID) -> Question -> f Question
integrationUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> U.UUID
    get q@IntegrationQuestion {} = _integrationUuid q
    get q = U.nil
    set :: Question -> U.UUID -> Question
    set q@IntegrationQuestion {} newValue = q { _integrationUuid = newValue }
    set q newValue = q

-- ------------------------------------------------------------------------------------------
integrationProps' :: Functor f => (M.Map String String -> f (M.Map String String)) -> Question -> f Question
integrationProps' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> M.Map String String
    get q@IntegrationQuestion {} = q ^. integrationProps
    get q = M.empty
    set :: Question -> M.Map String String -> Question
    set q@IntegrationQuestion {} newValue = q & integrationProps .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
targetUuid' :: Functor f => (U.UUID -> f U.UUID) -> Reference -> f Reference
targetUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Reference -> U.UUID
    get ref@CrossReference {} = _targetUuid ref
    set :: Reference -> U.UUID -> Reference
    set ref@CrossReference {} newValue = ref { _targetUuid = newValue }
    set ref newValue = ref