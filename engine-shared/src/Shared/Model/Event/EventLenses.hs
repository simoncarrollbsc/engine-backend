module Shared.Model.Event.EventLenses where

import qualified Data.UUID as U

import Shared.LensesClasses
import Shared.Model.Event.Event
import Shared.Model.Event.EventField

instance HasUuid' Event U.UUID where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> U.UUID
      get entity@AddKnowledgeModelEvent {} = _addKnowledgeModelEventUuid entity
      get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventUuid entity
      get entity@AddChapterEvent {} = _addChapterEventUuid entity
      get entity@EditChapterEvent {} = _editChapterEventUuid entity
      get entity@DeleteChapterEvent {} = _deleteChapterEventUuid entity
      get entity@AddOptionsQuestionEvent {} = _addOptionsQuestionEventUuid entity
      get entity@AddListQuestionEvent {} = _addListQuestionEventUuid entity
      get entity@AddValueQuestionEvent {} = _addValueQuestionEventUuid entity
      get entity@AddIntegrationQuestionEvent {} = _addIntegrationQuestionEventUuid entity
      get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventUuid entity
      get entity@EditListQuestionEvent {} = _editListQuestionEventUuid entity
      get entity@EditValueQuestionEvent {} = _editValueQuestionEventUuid entity
      get entity@EditIntegrationQuestionEvent {} = _editIntegrationQuestionEventUuid entity
      get entity@DeleteQuestionEvent {} = _deleteQuestionEventUuid entity
      get entity@AddAnswerEvent {} = _addAnswerEventUuid entity
      get entity@EditAnswerEvent {} = _editAnswerEventUuid entity
      get entity@DeleteAnswerEvent {} = _deleteAnswerEventUuid entity
      get entity@AddExpertEvent {} = _addExpertEventUuid entity
      get entity@EditExpertEvent {} = _editExpertEventUuid entity
      get entity@DeleteExpertEvent {} = _deleteExpertEventUuid entity
      get entity@AddResourcePageReferenceEvent {} = _addResourcePageReferenceEventUuid entity
      get entity@AddURLReferenceEvent {} = _addURLReferenceEventUuid entity
      get entity@AddCrossReferenceEvent {} = _addCrossReferenceEventUuid entity
      get entity@EditResourcePageReferenceEvent {} = _editResourcePageReferenceEventUuid entity
      get entity@EditURLReferenceEvent {} = _editURLReferenceEventUuid entity
      get entity@EditCrossReferenceEvent {} = _editCrossReferenceEventUuid entity
      get entity@DeleteReferenceEvent {} = _deleteReferenceEventUuid entity
      get entity@AddTagEvent {} = _addTagEventUuid entity
      get entity@EditTagEvent {} = _editTagEventUuid entity
      get entity@DeleteTagEvent {} = _deleteTagEventUuid entity
      get entity@AddIntegrationEvent {} = _addIntegrationEventUuid entity
      get entity@EditIntegrationEvent {} = _editIntegrationEventUuid entity
      get entity@DeleteIntegrationEvent {} = _deleteIntegrationEventUuid entity
      get entity@MoveQuestionEvent {} = _moveQuestionEventUuid entity
      get entity@MoveAnswerEvent {} = _moveAnswerEventUuid entity
      get entity@MoveExpertEvent {} = _moveExpertEventUuid entity
      get entity@MoveReferenceEvent {} = _moveReferenceEventUuid entity
      set :: Event -> U.UUID -> Event
      set entity@AddKnowledgeModelEvent {} newValue = entity {_addKnowledgeModelEventUuid = newValue}
      set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventUuid = newValue}
      set entity@AddChapterEvent {} newValue = entity {_addChapterEventUuid = newValue}
      set entity@EditChapterEvent {} newValue = entity {_editChapterEventUuid = newValue}
      set entity@DeleteChapterEvent {} newValue = entity {_deleteChapterEventUuid = newValue}
      set entity@AddOptionsQuestionEvent {} newValue = entity {_addOptionsQuestionEventUuid = newValue}
      set entity@AddListQuestionEvent {} newValue = entity {_addListQuestionEventUuid = newValue}
      set entity@AddValueQuestionEvent {} newValue = entity {_addValueQuestionEventUuid = newValue}
      set entity@AddIntegrationQuestionEvent {} newValue = entity {_addIntegrationQuestionEventUuid = newValue}
      set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventUuid = newValue}
      set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventUuid = newValue}
      set entity@EditValueQuestionEvent {} newValue = entity {_editValueQuestionEventUuid = newValue}
      set entity@EditIntegrationQuestionEvent {} newValue = entity {_editIntegrationQuestionEventUuid = newValue}
      set entity@DeleteQuestionEvent {} newValue = entity {_deleteQuestionEventUuid = newValue}
      set entity@AddAnswerEvent {} newValue = entity {_addAnswerEventUuid = newValue}
      set entity@EditAnswerEvent {} newValue = entity {_editAnswerEventUuid = newValue}
      set entity@DeleteAnswerEvent {} newValue = entity {_deleteAnswerEventUuid = newValue}
      set entity@AddExpertEvent {} newValue = entity {_addExpertEventUuid = newValue}
      set entity@EditExpertEvent {} newValue = entity {_editExpertEventUuid = newValue}
      set entity@DeleteExpertEvent {} newValue = entity {_deleteExpertEventUuid = newValue}
      set entity@AddResourcePageReferenceEvent {} newValue = entity {_addResourcePageReferenceEventUuid = newValue}
      set entity@AddURLReferenceEvent {} newValue = entity {_addURLReferenceEventUuid = newValue}
      set entity@AddCrossReferenceEvent {} newValue = entity {_addCrossReferenceEventUuid = newValue}
      set entity@EditResourcePageReferenceEvent {} newValue = entity {_editResourcePageReferenceEventUuid = newValue}
      set entity@EditURLReferenceEvent {} newValue = entity {_editURLReferenceEventUuid = newValue}
      set entity@EditCrossReferenceEvent {} newValue = entity {_editCrossReferenceEventUuid = newValue}
      set entity@DeleteReferenceEvent {} newValue = entity {_deleteReferenceEventUuid = newValue}
      set entity@AddTagEvent {} newValue = entity {_addTagEventUuid = newValue}
      set entity@EditTagEvent {} newValue = entity {_editTagEventUuid = newValue}
      set entity@DeleteTagEvent {} newValue = entity {_deleteTagEventUuid = newValue}
      set entity@AddIntegrationEvent {} newValue = entity {_addIntegrationEventUuid = newValue}
      set entity@EditIntegrationEvent {} newValue = entity {_editIntegrationEventUuid = newValue}
      set entity@DeleteIntegrationEvent {} newValue = entity {_deleteIntegrationEventUuid = newValue}
      set entity@MoveQuestionEvent {} newValue = entity {_moveQuestionEventUuid = newValue}
      set entity@MoveAnswerEvent {} newValue = entity {_moveAnswerEventUuid = newValue}
      set entity@MoveExpertEvent {} newValue = entity {_moveExpertEventUuid = newValue}
      set entity@MoveReferenceEvent {} newValue = entity {_moveReferenceEventUuid = newValue}

parentUuid' :: Functor f => (U.UUID -> f U.UUID) -> Event -> f Event
parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Event -> U.UUID
    get entity@AddKnowledgeModelEvent {} = _addKnowledgeModelEventParentUuid entity
    get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventParentUuid entity
    get entity@AddChapterEvent {} = _addChapterEventParentUuid entity
    get entity@EditChapterEvent {} = _editChapterEventParentUuid entity
    get entity@DeleteChapterEvent {} = _deleteChapterEventParentUuid entity
    get entity@AddOptionsQuestionEvent {} = _addOptionsQuestionEventParentUuid entity
    get entity@AddListQuestionEvent {} = _addListQuestionEventParentUuid entity
    get entity@AddValueQuestionEvent {} = _addValueQuestionEventParentUuid entity
    get entity@AddIntegrationQuestionEvent {} = _addIntegrationQuestionEventParentUuid entity
    get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventParentUuid entity
    get entity@EditListQuestionEvent {} = _editListQuestionEventParentUuid entity
    get entity@EditValueQuestionEvent {} = _editValueQuestionEventParentUuid entity
    get entity@EditIntegrationQuestionEvent {} = _editIntegrationQuestionEventParentUuid entity
    get entity@DeleteQuestionEvent {} = _deleteQuestionEventParentUuid entity
    get entity@AddAnswerEvent {} = _addAnswerEventParentUuid entity
    get entity@EditAnswerEvent {} = _editAnswerEventParentUuid entity
    get entity@DeleteAnswerEvent {} = _deleteAnswerEventParentUuid entity
    get entity@AddExpertEvent {} = _addExpertEventParentUuid entity
    get entity@EditExpertEvent {} = _editExpertEventParentUuid entity
    get entity@DeleteExpertEvent {} = _deleteExpertEventParentUuid entity
    get entity@AddResourcePageReferenceEvent {} = _addResourcePageReferenceEventParentUuid entity
    get entity@AddURLReferenceEvent {} = _addURLReferenceEventParentUuid entity
    get entity@AddCrossReferenceEvent {} = _addCrossReferenceEventParentUuid entity
    get entity@EditResourcePageReferenceEvent {} = _editResourcePageReferenceEventParentUuid entity
    get entity@EditURLReferenceEvent {} = _editURLReferenceEventParentUuid entity
    get entity@EditCrossReferenceEvent {} = _editCrossReferenceEventParentUuid entity
    get entity@DeleteReferenceEvent {} = _deleteReferenceEventParentUuid entity
    get entity@AddTagEvent {} = _addTagEventParentUuid entity
    get entity@EditTagEvent {} = _editTagEventParentUuid entity
    get entity@DeleteTagEvent {} = _deleteTagEventParentUuid entity
    get entity@AddIntegrationEvent {} = _addIntegrationEventParentUuid entity
    get entity@EditIntegrationEvent {} = _editIntegrationEventParentUuid entity
    get entity@DeleteIntegrationEvent {} = _deleteIntegrationEventParentUuid entity
    get entity@MoveQuestionEvent {} = _moveQuestionEventParentUuid entity
    get entity@MoveAnswerEvent {} = _moveAnswerEventParentUuid entity
    get entity@MoveExpertEvent {} = _moveExpertEventParentUuid entity
    get entity@MoveReferenceEvent {} = _moveReferenceEventParentUuid entity
    set :: Event -> U.UUID -> Event
    set entity@AddKnowledgeModelEvent {} newValue = entity {_addKnowledgeModelEventParentUuid = newValue}
    set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventParentUuid = newValue}
    set entity@AddChapterEvent {} newValue = entity {_addChapterEventParentUuid = newValue}
    set entity@EditChapterEvent {} newValue = entity {_editChapterEventParentUuid = newValue}
    set entity@DeleteChapterEvent {} newValue = entity {_deleteChapterEventParentUuid = newValue}
    set entity@AddOptionsQuestionEvent {} newValue = entity {_addOptionsQuestionEventParentUuid = newValue}
    set entity@AddListQuestionEvent {} newValue = entity {_addListQuestionEventParentUuid = newValue}
    set entity@AddValueQuestionEvent {} newValue = entity {_addValueQuestionEventParentUuid = newValue}
    set entity@AddIntegrationQuestionEvent {} newValue = entity {_addIntegrationQuestionEventParentUuid = newValue}
    set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventParentUuid = newValue}
    set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventParentUuid = newValue}
    set entity@EditValueQuestionEvent {} newValue = entity {_editValueQuestionEventParentUuid = newValue}
    set entity@EditIntegrationQuestionEvent {} newValue = entity {_editIntegrationQuestionEventParentUuid = newValue}
    set entity@DeleteQuestionEvent {} newValue = entity {_deleteQuestionEventParentUuid = newValue}
    set entity@AddAnswerEvent {} newValue = entity {_addAnswerEventParentUuid = newValue}
    set entity@EditAnswerEvent {} newValue = entity {_editAnswerEventParentUuid = newValue}
    set entity@DeleteAnswerEvent {} newValue = entity {_deleteAnswerEventParentUuid = newValue}
    set entity@AddExpertEvent {} newValue = entity {_addExpertEventParentUuid = newValue}
    set entity@EditExpertEvent {} newValue = entity {_editExpertEventParentUuid = newValue}
    set entity@DeleteExpertEvent {} newValue = entity {_deleteExpertEventParentUuid = newValue}
    set entity@AddResourcePageReferenceEvent {} newValue = entity {_addResourcePageReferenceEventParentUuid = newValue}
    set entity@AddURLReferenceEvent {} newValue = entity {_addURLReferenceEventParentUuid = newValue}
    set entity@AddCrossReferenceEvent {} newValue = entity {_addCrossReferenceEventParentUuid = newValue}
    set entity@EditResourcePageReferenceEvent {} newValue =
      entity {_editResourcePageReferenceEventParentUuid = newValue}
    set entity@EditURLReferenceEvent {} newValue = entity {_editURLReferenceEventParentUuid = newValue}
    set entity@EditCrossReferenceEvent {} newValue = entity {_editCrossReferenceEventParentUuid = newValue}
    set entity@DeleteReferenceEvent {} newValue = entity {_deleteReferenceEventParentUuid = newValue}
    set entity@AddTagEvent {} newValue = entity {_addTagEventParentUuid = newValue}
    set entity@EditTagEvent {} newValue = entity {_editTagEventParentUuid = newValue}
    set entity@DeleteTagEvent {} newValue = entity {_deleteTagEventParentUuid = newValue}
    set entity@AddIntegrationEvent {} newValue = entity {_addIntegrationEventParentUuid = newValue}
    set entity@EditIntegrationEvent {} newValue = entity {_editIntegrationEventParentUuid = newValue}
    set entity@DeleteIntegrationEvent {} newValue = entity {_deleteIntegrationEventParentUuid = newValue}
    set entity@MoveQuestionEvent {} newValue = entity {_moveQuestionEventParentUuid = newValue}
    set entity@MoveAnswerEvent {} newValue = entity {_moveAnswerEventParentUuid = newValue}
    set entity@MoveExpertEvent {} newValue = entity {_moveExpertEventParentUuid = newValue}
    set entity@MoveReferenceEvent {} newValue = entity {_moveReferenceEventParentUuid = newValue}

entityUuid' :: Functor f => (U.UUID -> f U.UUID) -> Event -> f Event
entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Event -> U.UUID
    get entity@AddKnowledgeModelEvent {} = _addKnowledgeModelEventEntityUuid entity
    get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventEntityUuid entity
    get entity@AddChapterEvent {} = _addChapterEventEntityUuid entity
    get entity@EditChapterEvent {} = _editChapterEventEntityUuid entity
    get entity@DeleteChapterEvent {} = _deleteChapterEventEntityUuid entity
    get entity@AddOptionsQuestionEvent {} = _addOptionsQuestionEventEntityUuid entity
    get entity@AddListQuestionEvent {} = _addListQuestionEventEntityUuid entity
    get entity@AddValueQuestionEvent {} = _addValueQuestionEventEntityUuid entity
    get entity@AddIntegrationQuestionEvent {} = _addIntegrationQuestionEventEntityUuid entity
    get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventEntityUuid entity
    get entity@EditListQuestionEvent {} = _editListQuestionEventEntityUuid entity
    get entity@EditValueQuestionEvent {} = _editValueQuestionEventEntityUuid entity
    get entity@EditIntegrationQuestionEvent {} = _editIntegrationQuestionEventEntityUuid entity
    get entity@DeleteQuestionEvent {} = _deleteQuestionEventEntityUuid entity
    get entity@AddAnswerEvent {} = _addAnswerEventEntityUuid entity
    get entity@EditAnswerEvent {} = _editAnswerEventEntityUuid entity
    get entity@DeleteAnswerEvent {} = _deleteAnswerEventEntityUuid entity
    get entity@AddExpertEvent {} = _addExpertEventEntityUuid entity
    get entity@EditExpertEvent {} = _editExpertEventEntityUuid entity
    get entity@DeleteExpertEvent {} = _deleteExpertEventEntityUuid entity
    get entity@AddResourcePageReferenceEvent {} = _addResourcePageReferenceEventEntityUuid entity
    get entity@AddURLReferenceEvent {} = _addURLReferenceEventEntityUuid entity
    get entity@AddCrossReferenceEvent {} = _addCrossReferenceEventEntityUuid entity
    get entity@EditResourcePageReferenceEvent {} = _editResourcePageReferenceEventEntityUuid entity
    get entity@EditURLReferenceEvent {} = _editURLReferenceEventEntityUuid entity
    get entity@EditCrossReferenceEvent {} = _editCrossReferenceEventEntityUuid entity
    get entity@DeleteReferenceEvent {} = _deleteReferenceEventEntityUuid entity
    get entity@AddTagEvent {} = _addTagEventEntityUuid entity
    get entity@EditTagEvent {} = _editTagEventEntityUuid entity
    get entity@DeleteTagEvent {} = _deleteTagEventEntityUuid entity
    get entity@AddIntegrationEvent {} = _addIntegrationEventEntityUuid entity
    get entity@EditIntegrationEvent {} = _editIntegrationEventEntityUuid entity
    get entity@DeleteIntegrationEvent {} = _deleteIntegrationEventEntityUuid entity
    get entity@MoveQuestionEvent {} = _moveQuestionEventEntityUuid entity
    get entity@MoveAnswerEvent {} = _moveAnswerEventEntityUuid entity
    get entity@MoveExpertEvent {} = _moveExpertEventEntityUuid entity
    get entity@MoveReferenceEvent {} = _moveReferenceEventEntityUuid entity
    set :: Event -> U.UUID -> Event
    set entity@AddKnowledgeModelEvent {} newValue = entity {_addKnowledgeModelEventEntityUuid = newValue}
    set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventEntityUuid = newValue}
    set entity@AddChapterEvent {} newValue = entity {_addChapterEventEntityUuid = newValue}
    set entity@EditChapterEvent {} newValue = entity {_editChapterEventEntityUuid = newValue}
    set entity@DeleteChapterEvent {} newValue = entity {_deleteChapterEventEntityUuid = newValue}
    set entity@AddOptionsQuestionEvent {} newValue = entity {_addOptionsQuestionEventEntityUuid = newValue}
    set entity@AddListQuestionEvent {} newValue = entity {_addListQuestionEventEntityUuid = newValue}
    set entity@AddValueQuestionEvent {} newValue = entity {_addValueQuestionEventEntityUuid = newValue}
    set entity@AddIntegrationQuestionEvent {} newValue = entity {_addIntegrationQuestionEventEntityUuid = newValue}
    set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventEntityUuid = newValue}
    set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventEntityUuid = newValue}
    set entity@EditValueQuestionEvent {} newValue = entity {_editValueQuestionEventEntityUuid = newValue}
    set entity@EditIntegrationQuestionEvent {} newValue = entity {_editIntegrationQuestionEventEntityUuid = newValue}
    set entity@DeleteQuestionEvent {} newValue = entity {_deleteQuestionEventEntityUuid = newValue}
    set entity@AddAnswerEvent {} newValue = entity {_addAnswerEventEntityUuid = newValue}
    set entity@EditAnswerEvent {} newValue = entity {_editAnswerEventEntityUuid = newValue}
    set entity@DeleteAnswerEvent {} newValue = entity {_deleteAnswerEventEntityUuid = newValue}
    set entity@AddExpertEvent {} newValue = entity {_addExpertEventEntityUuid = newValue}
    set entity@EditExpertEvent {} newValue = entity {_editExpertEventEntityUuid = newValue}
    set entity@DeleteExpertEvent {} newValue = entity {_deleteExpertEventEntityUuid = newValue}
    set entity@AddResourcePageReferenceEvent {} newValue = entity {_addResourcePageReferenceEventEntityUuid = newValue}
    set entity@AddURLReferenceEvent {} newValue = entity {_addURLReferenceEventEntityUuid = newValue}
    set entity@AddCrossReferenceEvent {} newValue = entity {_addCrossReferenceEventEntityUuid = newValue}
    set entity@EditResourcePageReferenceEvent {} newValue =
      entity {_editResourcePageReferenceEventEntityUuid = newValue}
    set entity@EditURLReferenceEvent {} newValue = entity {_editURLReferenceEventEntityUuid = newValue}
    set entity@EditCrossReferenceEvent {} newValue = entity {_editCrossReferenceEventEntityUuid = newValue}
    set entity@DeleteReferenceEvent {} newValue = entity {_deleteReferenceEventEntityUuid = newValue}
    set entity@AddTagEvent {} newValue = entity {_addTagEventEntityUuid = newValue}
    set entity@EditTagEvent {} newValue = entity {_editTagEventEntityUuid = newValue}
    set entity@DeleteTagEvent {} newValue = entity {_deleteTagEventEntityUuid = newValue}
    set entity@AddIntegrationEvent {} newValue = entity {_addIntegrationEventEntityUuid = newValue}
    set entity@EditIntegrationEvent {} newValue = entity {_editIntegrationEventEntityUuid = newValue}
    set entity@DeleteIntegrationEvent {} newValue = entity {_deleteIntegrationEventEntityUuid = newValue}
    set entity@MoveQuestionEvent {} newValue = entity {_moveQuestionEventEntityUuid = newValue}
    set entity@MoveAnswerEvent {} newValue = entity {_moveAnswerEventEntityUuid = newValue}
    set entity@MoveExpertEvent {} newValue = entity {_moveExpertEventEntityUuid = newValue}
    set entity@MoveReferenceEvent {} newValue = entity {_moveReferenceEventEntityUuid = newValue}

instance HasChapterUuids' Event (EventField [U.UUID]) where
  chapterUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventChapterUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventChapterUuids = newValue}
      set entity newValue = entity

instance HasTagUuids' Event (EventField [U.UUID]) where
  tagUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventTagUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventTagUuids = newValue}
      set entity newValue = entity

instance HasIntegrationUuids' Event (EventField [U.UUID]) where
  integrationUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditKnowledgeModelEvent {} = _editKnowledgeModelEventIntegrationUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditKnowledgeModelEvent {} newValue = entity {_editKnowledgeModelEventIntegrationUuids = newValue}
      set entity newValue = entity

instance HasQuestionUuids' Event (EventField [U.UUID]) where
  questionUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditChapterEvent {} = _editChapterEventQuestionUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditChapterEvent {} newValue = entity {_editChapterEventQuestionUuids = newValue}
      set entity newValue = entity

instance HasExpertUuids' Event (EventField [U.UUID]) where
  expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventReferenceUuids entity
      get entity@EditListQuestionEvent {} = _editListQuestionEventReferenceUuids entity
      get entity@EditValueQuestionEvent {} = _editValueQuestionEventReferenceUuids entity
      get entity@EditIntegrationQuestionEvent {} = _editIntegrationQuestionEventReferenceUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventReferenceUuids = newValue}
      set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventReferenceUuids = newValue}
      set entity@EditValueQuestionEvent {} newValue = entity {_editValueQuestionEventReferenceUuids = newValue}
      set entity@EditIntegrationQuestionEvent {} newValue =
        entity {_editIntegrationQuestionEventReferenceUuids = newValue}
      set entity newValue = entity

instance HasReferenceUuids' Event (EventField [U.UUID]) where
  referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventReferenceUuids entity
      get entity@EditListQuestionEvent {} = _editListQuestionEventReferenceUuids entity
      get entity@EditValueQuestionEvent {} = _editValueQuestionEventReferenceUuids entity
      get entity@EditIntegrationQuestionEvent {} = _editIntegrationQuestionEventReferenceUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventReferenceUuids = newValue}
      set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventReferenceUuids = newValue}
      set entity@EditValueQuestionEvent {} newValue = entity {_editValueQuestionEventReferenceUuids = newValue}
      set entity@EditIntegrationQuestionEvent {} newValue =
        entity {_editIntegrationQuestionEventReferenceUuids = newValue}
      set entity newValue = entity

instance HasItemTemplateQuestionUuids' Event (EventField [U.UUID]) where
  itemTemplateQuestionUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditListQuestionEvent {} = _editListQuestionEventItemTemplateQuestionUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditListQuestionEvent {} newValue = entity {_editListQuestionEventItemTemplateQuestionUuids = newValue}
      set entity newValue = entity

instance HasAnswerUuids' Event (EventField [U.UUID]) where
  answerUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditOptionsQuestionEvent {} = _editOptionsQuestionEventAnswerUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditOptionsQuestionEvent {} newValue = entity {_editOptionsQuestionEventAnswerUuids = newValue}
      set entity newValue = entity

instance HasFollowUpsUuids' Event (EventField [U.UUID]) where
  followUpUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField [U.UUID]
      get entity@EditAnswerEvent {} = _editAnswerEventFollowUpUuids entity
      get entity = NothingChanged
      set :: Event -> EventField [U.UUID] -> Event
      set entity@EditAnswerEvent {} newValue = entity {_editAnswerEventFollowUpUuids = newValue}
      set entity newValue = entity

instance HasShortUuid' Event String where
  shortUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> String
      get entity@AddResourcePageReferenceEvent {} = _addResourcePageReferenceEventShortUuid entity
      get entity = ""
      set :: Event -> String -> Event
      set entity@AddResourcePageReferenceEvent {} newValue = entity {_addResourcePageReferenceEventShortUuid = newValue}
      set entity newValue = entity

instance HasUrl' Event String where
  url' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> String
      get entity@AddURLReferenceEvent {} = _addURLReferenceEventUrl entity
      get entity = ""
      set :: Event -> String -> Event
      set entity@AddURLReferenceEvent {} newValue = entity {_addURLReferenceEventUrl = newValue}
      set entity newValue = entity

instance HasLabel' Event String where
  label' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> String
      get entity@AddURLReferenceEvent {} = _addURLReferenceEventLabel entity
      get entity = ""
      set :: Event -> String -> Event
      set entity@AddURLReferenceEvent {} newValue = entity {_addURLReferenceEventLabel = newValue}
      set entity newValue = entity

instance HasTargetUuid' Event U.UUID where
  targetUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> U.UUID
      get entity@AddCrossReferenceEvent {} = _addCrossReferenceEventTargetUuid entity
      get entity@MoveQuestionEvent {} = _moveQuestionEventTargetUuid entity
      get entity@MoveAnswerEvent {} = _moveAnswerEventTargetUuid entity
      get entity@MoveExpertEvent {} = _moveExpertEventTargetUuid entity
      get entity@MoveReferenceEvent {} = _moveReferenceEventTargetUuid entity
      get entity = U.nil
      set :: Event -> U.UUID -> Event
      set entity@AddCrossReferenceEvent {} newValue = entity {_addCrossReferenceEventTargetUuid = newValue}
      set entity@MoveQuestionEvent {} newValue = entity {_moveQuestionEventTargetUuid = newValue}
      set entity@MoveAnswerEvent {} newValue = entity {_moveAnswerEventTargetUuid = newValue}
      set entity@MoveExpertEvent {} newValue = entity {_moveExpertEventTargetUuid = newValue}
      set entity@MoveReferenceEvent {} newValue = entity {_moveReferenceEventTargetUuid = newValue}
      set entity newValue = entity

instance HasTargetUuid' Event (EventField U.UUID) where
  targetUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> EventField U.UUID
      get entity@EditCrossReferenceEvent {} = _editCrossReferenceEventTargetUuid entity
      get entity = NothingChanged
      set :: Event -> EventField U.UUID -> Event
      set entity@EditCrossReferenceEvent {} newValue = entity {_editCrossReferenceEventTargetUuid = newValue}
      set entity newValue = entity

instance HasDescription' Event String where
  description' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> String
      get entity@AddCrossReferenceEvent {} = _addCrossReferenceEventDescription entity
      get entity = ""
      set :: Event -> String -> Event
      set entity@AddCrossReferenceEvent {} newValue = entity {_addCrossReferenceEventDescription = newValue}
      set entity newValue = entity
