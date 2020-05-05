module Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddReferenceEvent Reference where
  createEntity (AddResourcePageReferenceEvent' e) =
    ResourcePageReference' $
    ResourcePageReference
      {_uuid = e ^. entityUuid, _shortUuid = e ^. shortUuid}
  createEntity (AddURLReferenceEvent' e) =
    URLReference' $
    URLReference {_uuid = e ^. entityUuid, _url = e ^. url, _label = e ^. label}
  createEntity (AddCrossReferenceEvent' e) =
    CrossReference' $
    CrossReference
      { _uuid = e ^. entityUuid
      , _targetUuid = e ^. targetUuid
      , _description = e ^. description
      }

instance EditEntity EditReferenceEvent Reference where
  editEntity e' ref =
    case e' of
      (EditResourcePageReferenceEvent' e) ->
        ResourcePageReference' . applyToResourcePageReference e . convertToResourcePageReference $ ref
      (EditURLReferenceEvent' e) -> URLReference' . applyToURLReference e . convertToURLReference $ ref
      (EditCrossReferenceEvent' e) -> CrossReference' . applyToCrossReference e . convertToCrossReference $ ref
    where
      applyToResourcePageReference e = applyShortUuid e
      applyToURLReference e = applyAnchor e . applyUrl e
      applyToCrossReference e = applyDescription e . applyTarget e
      applyShortUuid e ref = applyValue (e ^. shortUuid) ref shortUuid
      applyUrl e ref = applyValue (e ^. url) ref url
      applyAnchor e ref = applyValue (e ^. label) ref label
      applyTarget e ref = applyValue (e ^. targetUuid) ref targetUuid
      applyDescription e ref = applyValue (e ^. description) ref description

convertToResourcePageReference :: Reference -> ResourcePageReference
convertToResourcePageReference (ResourcePageReference' ref) = ref
convertToResourcePageReference ref' =
  case ref' of
    (URLReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      ResourcePageReference {_uuid = ref ^. uuid, _shortUuid = ""}

convertToURLReference :: Reference -> URLReference
convertToURLReference (URLReference' ref) = ref
convertToURLReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref = URLReference {_uuid = ref ^. uuid, _url = "", _label = ""}

convertToCrossReference :: Reference -> CrossReference
convertToCrossReference (CrossReference' ref) = ref
convertToCrossReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (URLReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      CrossReference
        {_uuid = ref ^. uuid, _targetUuid = U.nil, _description = ""}
