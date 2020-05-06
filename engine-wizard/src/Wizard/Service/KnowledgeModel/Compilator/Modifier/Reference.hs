module Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesExtension
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity Event Reference where
  createEntity e@AddResourcePageReferenceEvent {..} =
    ResourcePageReference
      { _resourcePageReferenceUuid = _addResourcePageReferenceEventEntityUuid
      , _resourcePageReferenceShortUuid = _addResourcePageReferenceEventShortUuid
      }
  createEntity e@AddURLReferenceEvent {..} =
    URLReference
      { _uRLReferenceUuid = _addURLReferenceEventEntityUuid
      , _uRLReferenceUrl = _addURLReferenceEventUrl
      , _uRLReferenceLabel = _addURLReferenceEventLabel
      }
  createEntity e@AddCrossReferenceEvent {..} =
    CrossReference
      { _crossReferenceUuid = _addCrossReferenceEventEntityUuid
      , _crossReferenceTargetUuid = _addCrossReferenceEventTargetUuid
      , _crossReferenceDescription = _addCrossReferenceEventDescription
      }

instance EditEntity Event Reference where
  editEntity e' ref =
    case e' of
      e@EditResourcePageReferenceEvent {} -> applyToResourcePageReference e . convertToResourcePageReference $ ref
      e@EditURLReferenceEvent {} -> applyToURLReference e . convertToURLReference $ ref
      e@EditCrossReferenceEvent {} -> applyToCrossReference e . convertToCrossReference $ ref
    where
      applyToResourcePageReference e = applyShortUuid e
      applyToURLReference e = applyLabel e . applyUrl e
      applyToCrossReference e = applyDescription e . applyTarget e
      applyShortUuid e ref =  
        case _editResourcePageReferenceEventShortUuid e of
          ChangedValue newValue -> ref {_resourcePageReferenceShortUuid = newValue}
          NothingChanged -> ref
      applyUrl e ref =  
        case _editURLReferenceEventUrl e of
          ChangedValue newValue -> ref {_uRLReferenceUrl = newValue}
          NothingChanged -> ref
      applyLabel e ref = 
        case _editURLReferenceEventLabel e of
          ChangedValue newValue -> ref {_uRLReferenceLabel = newValue}
          NothingChanged -> ref
      applyTarget e ref = 
        case _editCrossReferenceEventTargetUuid e of
          ChangedValue newValue -> ref {_crossReferenceTargetUuid = newValue}
          NothingChanged -> ref
      applyDescription e ref =
        case _editCrossReferenceEventDescription e of
          ChangedValue description -> ref {_crossReferenceDescription = description}
          NothingChanged -> ref

convertToResourcePageReference :: Reference -> Reference
convertToResourcePageReference ref@ResourcePageReference {} = ref
convertToResourcePageReference ref' =
  case ref' of
    ref@URLReference {} -> createQuestion ref
    ref@CrossReference {} -> createQuestion ref
  where
    createQuestion ref =
      ResourcePageReference {_resourcePageReferenceUuid = ref ^. uuid', _resourcePageReferenceShortUuid = ""}

convertToURLReference :: Reference -> Reference
convertToURLReference ref@URLReference {} = ref
convertToURLReference ref' =
  case ref' of
    ref@ResourcePageReference {} -> createQuestion ref
    ref@CrossReference {} -> createQuestion ref
  where
    createQuestion ref = URLReference {_uRLReferenceUuid = ref ^. uuid', _uRLReferenceUrl = "", _uRLReferenceLabel = ""}

convertToCrossReference :: Reference -> Reference
convertToCrossReference ref@CrossReference {} = ref
convertToCrossReference ref' =
  case ref' of
    ref@ResourcePageReference {} -> createQuestion ref
    ref@URLReference {} -> createQuestion ref
  where
    createQuestion ref =
      CrossReference
        {_crossReferenceUuid = ref ^. uuid', _crossReferenceTargetUuid = U.nil, _crossReferenceDescription = ""}