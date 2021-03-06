module Shared.Model.KnowledgeModel.KnowledgeModelLenses
  ( module Shared.Model.Common.Lens
  , chaptersL
  , chaptersM
  , questionsL
  , questionsM
  , answersL
  , answersM
  , choicesL
  , choicesM
  , expertsL
  , expertsM
  , referencesL
  , referencesM
  , integrationsL
  , integrationsM
  , tagsL
  , tagsM
  , createEntityLFn
  , createEntityMFn
  , toMap
  , title'
  , text'
  , requiredLevel'
  , tagUuids'
  , answerUuids'
  , choiceUuids'
  , itemTemplateQuestionUuids'
  , valueType'
  , integrationUuid'
  , props'
  ) where

import Control.Lens hiding (Choice)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
chaptersL :: Functor f => ([Chapter] -> f [Chapter]) -> KnowledgeModel -> f KnowledgeModel
chaptersL = createEntityLFn (entities . chapters)

chaptersM :: Functor f => (M.Map U.UUID Chapter -> f (M.Map U.UUID Chapter)) -> KnowledgeModel -> f KnowledgeModel
chaptersM = createEntityMFn (entities . chapters)

------------------------------------------------------------------------------------------
questionsL :: Functor f => ([Question] -> f [Question]) -> KnowledgeModel -> f KnowledgeModel
questionsL = createEntityLFn (entities . questions)

questionsM :: Functor f => (M.Map U.UUID Question -> f (M.Map U.UUID Question)) -> KnowledgeModel -> f KnowledgeModel
questionsM = createEntityMFn (entities . questions)

------------------------------------------------------------------------------------------
answersL :: Functor f => ([Answer] -> f [Answer]) -> KnowledgeModel -> f KnowledgeModel
answersL = createEntityLFn (entities . answers)

answersM :: Functor f => (M.Map U.UUID Answer -> f (M.Map U.UUID Answer)) -> KnowledgeModel -> f KnowledgeModel
answersM = createEntityMFn (entities . answers)

------------------------------------------------------------------------------------------
choicesL :: Functor f => ([Choice] -> f [Choice]) -> KnowledgeModel -> f KnowledgeModel
choicesL = createEntityLFn (entities . choices)

choicesM :: Functor f => (M.Map U.UUID Choice -> f (M.Map U.UUID Choice)) -> KnowledgeModel -> f KnowledgeModel
choicesM = createEntityMFn (entities . choices)

------------------------------------------------------------------------------------------
expertsL :: Functor f => ([Expert] -> f [Expert]) -> KnowledgeModel -> f KnowledgeModel
expertsL = createEntityLFn (entities . experts)

expertsM :: Functor f => (M.Map U.UUID Expert -> f (M.Map U.UUID Expert)) -> KnowledgeModel -> f KnowledgeModel
expertsM = createEntityMFn (entities . experts)

------------------------------------------------------------------------------------------
referencesL :: Functor f => ([Reference] -> f [Reference]) -> KnowledgeModel -> f KnowledgeModel
referencesL = createEntityLFn (entities . references)

referencesM :: Functor f => (M.Map U.UUID Reference -> f (M.Map U.UUID Reference)) -> KnowledgeModel -> f KnowledgeModel
referencesM = createEntityMFn (entities . references)

------------------------------------------------------------------------------------------
integrationsL :: Functor f => ([Integration] -> f [Integration]) -> KnowledgeModel -> f KnowledgeModel
integrationsL = createEntityLFn (entities . integrations)

integrationsM ::
     Functor f => (M.Map U.UUID Integration -> f (M.Map U.UUID Integration)) -> KnowledgeModel -> f KnowledgeModel
integrationsM = createEntityMFn (entities . integrations)

------------------------------------------------------------------------------------------
tagsL :: Functor f => ([Tag] -> f [Tag]) -> KnowledgeModel -> f KnowledgeModel
tagsL = createEntityLFn (entities . tags)

tagsM :: Functor f => (M.Map U.UUID Tag -> f (M.Map U.UUID Tag)) -> KnowledgeModel -> f KnowledgeModel
tagsM = createEntityMFn (entities . tags)

------------------------------------------------------------------------------------------
createEntityLFn ::
     (HasUuid' a, Functor f)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> ([a] -> f [a])
  -> KnowledgeModel
  -> f KnowledgeModel
createEntityLFn accessor convert km = fmap (update km) (convert . M.elems $ km ^. accessor)
  where
    update km newValue = km & accessor .~ toMap newValue

createEntityMFn ::
     Functor f
  => Lens' KnowledgeModel (M.Map U.UUID a)
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
      get (OptionsQuestion' entity) = entity ^. uuid
      get (MultiChoiceQuestion' entity) = entity ^. uuid
      get (ListQuestion' entity) = entity ^. uuid
      get (ValueQuestion' entity) = entity ^. uuid
      get (IntegrationQuestion' entity) = entity ^. uuid
      set :: Question -> U.UUID -> Question
      set (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity & uuid .~ newValue
      set (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity & uuid .~ newValue
      set (ListQuestion' entity) newValue = ListQuestion' $ entity & uuid .~ newValue
      set (ValueQuestion' entity) newValue = ValueQuestion' $ entity & uuid .~ newValue
      set (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity & uuid .~ newValue

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
      get (ResourcePageReference' entity) = entity ^. uuid
      get (URLReference' entity) = entity ^. uuid
      get (CrossReference' entity) = entity ^. uuid
      set :: Reference -> U.UUID -> Reference
      set (ResourcePageReference' entity) newValue = ResourcePageReference' $ entity & uuid .~ newValue
      set (URLReference' entity) newValue = URLReference' $ entity & uuid .~ newValue
      set (CrossReference' entity) newValue = CrossReference' $ entity & uuid .~ newValue

instance HasUuid' Answer where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Answer -> U.UUID
      get entity = entity ^. uuid
      set :: Answer -> U.UUID -> Answer
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' Choice where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Choice -> U.UUID
      get entity = entity ^. uuid
      set :: Choice -> U.UUID -> Choice
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
    get (OptionsQuestion' q) = q ^. title
    get (MultiChoiceQuestion' q) = q ^. title
    get (ListQuestion' q) = q ^. title
    get (ValueQuestion' q) = q ^. title
    get (IntegrationQuestion' q) = q ^. title
    set :: Question -> String -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & title .~ newValue
    set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & title .~ newValue
    set (ListQuestion' q) newValue = ListQuestion' $ q & title .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & title .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & title .~ newValue

------------------------------------------------------------------------------------------
text' :: Functor f => (Maybe String -> f (Maybe String)) -> Question -> f Question
text' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe String
    get (OptionsQuestion' q) = q ^. text
    get (MultiChoiceQuestion' q) = q ^. text
    get (ListQuestion' q) = q ^. text
    get (ValueQuestion' q) = q ^. text
    get (IntegrationQuestion' q) = q ^. text
    set :: Question -> Maybe String -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & text .~ newValue
    set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & text .~ newValue
    set (ListQuestion' q) newValue = ListQuestion' $ q & text .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & text .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & text .~ newValue

------------------------------------------------------------------------------------------
requiredLevel' :: Functor f => (Maybe Int -> f (Maybe Int)) -> Question -> f Question
requiredLevel' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> Maybe Int
    get (OptionsQuestion' q) = q ^. requiredLevel
    get (MultiChoiceQuestion' q) = q ^. requiredLevel
    get (ListQuestion' q) = q ^. requiredLevel
    get (ValueQuestion' q) = q ^. requiredLevel
    get (IntegrationQuestion' q) = q ^. requiredLevel
    set :: Question -> Maybe Int -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & requiredLevel .~ newValue
    set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & requiredLevel .~ newValue
    set (ListQuestion' q) newValue = ListQuestion' $ q & requiredLevel .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & requiredLevel .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & requiredLevel .~ newValue

------------------------------------------------------------------------------------------
tagUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
tagUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (OptionsQuestion' q) = q ^. tagUuids
    get (MultiChoiceQuestion' q) = q ^. tagUuids
    get (ListQuestion' q) = q ^. tagUuids
    get (ValueQuestion' q) = q ^. tagUuids
    get (IntegrationQuestion' q) = q ^. tagUuids
    set :: Question -> [U.UUID] -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & tagUuids .~ newValue
    set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & tagUuids .~ newValue
    set (ListQuestion' q) newValue = ListQuestion' $ q & tagUuids .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & tagUuids .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & tagUuids .~ newValue

-- ------------------------------------------------------------------------------------------
instance HasExpertUuids' Question [U.UUID] where
  expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get (OptionsQuestion' entity) = entity ^. expertUuids
      get (MultiChoiceQuestion' entity) = entity ^. expertUuids
      get (ListQuestion' entity) = entity ^. expertUuids
      get (ValueQuestion' entity) = entity ^. expertUuids
      get (IntegrationQuestion' entity) = entity ^. expertUuids
      set :: Question -> [U.UUID] -> Question
      set (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity & expertUuids .~ newValue
      set (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity & expertUuids .~ newValue
      set (ListQuestion' entity) newValue = ListQuestion' $ entity & expertUuids .~ newValue
      set (ValueQuestion' entity) newValue = ValueQuestion' $ entity & expertUuids .~ newValue
      set (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity & expertUuids .~ newValue

-- ------------------------------------------------------------------------------------------
instance HasReferenceUuids' Question [U.UUID] where
  referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Question -> [U.UUID]
      get (OptionsQuestion' q) = q ^. referenceUuids
      get (MultiChoiceQuestion' q) = q ^. referenceUuids
      get (ListQuestion' q) = q ^. referenceUuids
      get (ValueQuestion' q) = q ^. referenceUuids
      get (IntegrationQuestion' q) = q ^. referenceUuids
      set :: Question -> [U.UUID] -> Question
      set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & referenceUuids .~ newValue
      set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & referenceUuids .~ newValue
      set (ListQuestion' q) newValue = ListQuestion' $ q & referenceUuids .~ newValue
      set (ValueQuestion' q) newValue = ValueQuestion' $ q & referenceUuids .~ newValue
      set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & referenceUuids .~ newValue

-- ------------------------------------------------------------------------------------------
answerUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
answerUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (OptionsQuestion' q) = q ^. answerUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & answerUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
choiceUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
choiceUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (MultiChoiceQuestion' q) = q ^. choiceUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q & choiceUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
itemTemplateQuestionUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
itemTemplateQuestionUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (ListQuestion' q) = q ^. itemTemplateQuestionUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set (ListQuestion' q) newValue = ListQuestion' $ q & itemTemplateQuestionUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
valueType' :: Functor f => (QuestionValueType -> f QuestionValueType) -> Question -> f Question
valueType' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> QuestionValueType
    get (ValueQuestion' q) = q ^. valueType
    get q = StringQuestionValueType
    set :: Question -> QuestionValueType -> Question
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & valueType .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
integrationUuid' :: Functor f => (U.UUID -> f U.UUID) -> Question -> f Question
integrationUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> U.UUID
    get (IntegrationQuestion' q) = q ^. integrationUuid
    get q = U.nil
    set :: Question -> U.UUID -> Question
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & integrationUuid .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
props' :: Functor f => (M.Map String String -> f (M.Map String String)) -> Question -> f Question
props' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> M.Map String String
    get (IntegrationQuestion' q) = q ^. props
    get q = M.empty
    set :: Question -> M.Map String String -> Question
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & props .~ newValue
    set q newValue = q
