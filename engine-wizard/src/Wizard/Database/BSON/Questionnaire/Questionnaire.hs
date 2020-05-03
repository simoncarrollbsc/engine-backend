module Wizard.Database.BSON.Questionnaire.Questionnaire where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe ()

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireAccessibility ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireLabel ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireReply ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire where
  toBSON Questionnaire {..} =
    [ "uuid" BSON.=: _uuid
    , "name" BSON.=: _name
    , "level" BSON.=: _level
    , "accessibility" BSON.=: _accessibility
    , "packageId" BSON.=: _packageId
    , "selectedTagUuids" BSON.=: _selectedTagUuids
    , "templateUuid" BSON.=: _templateUuid
    , "formatUuid" BSON.=: _formatUuid
    , "ownerUuid" BSON.=: _ownerUuid
    , "creatorUuid" BSON.=: _creatorUuid
    , "replies" BSON.=: _replies
    , "labels" BSON.=: _labels
    , "createdAt" BSON.=: _createdAt
    , "updatedAt" BSON.=: _updatedAt
    ]
  toBSON' Questionnaire {..} =
    [ "uuid" BSON.=: _uuid
    , "name" BSON.=: _name
    , "level" BSON.=: _level
    , "accessibility" BSON.=: _accessibility
    , "packageId" BSON.=: _packageId
    , "selectedTagUuids" BSON.=: _selectedTagUuids
    , "templateUuid" BSON.=: _templateUuid
    , "formatUuid" BSON.=: _formatUuid
    , "ownerUuid" BSON.=: _ownerUuid
    , "creatorUuid" BSON.=: _creatorUuid
    , "replies" BSON.=: _replies
    , "labels" BSON.=: _labels
    , "createdAt" BSON.=: _createdAt
    , "updatedAt" BSON.=: _updatedAt
    ]

instance FromBSON Questionnaire where
  fromBSON doc = do
    _uuid <- BSON.lookup "uuid" doc
    _name <- BSON.lookup "name" doc
    _level <- BSON.lookup "level" doc
    _accessibility <- BSON.lookup "accessibility" doc
    _packageId <- BSON.lookup "packageId" doc
    _selectedTagUuids <- BSON.lookup "selectedTagUuids" doc
    let _templateUuid = BSON.lookup "templateUuid" doc
    let _formatUuid = BSON.lookup "formatUuid" doc
    let _ownerUuid = BSON.lookup "ownerUuid" doc
    let _creatorUuid = BSON.lookup "creatorUuid" doc
    _replies <- BSON.lookup "replies" doc
    _labels <- BSON.lookup "labels" doc
    _createdAt <- BSON.lookup "createdAt" doc
    _updatedAt <- BSON.lookup "updatedAt" doc
    return Questionnaire {..}
  fromBSON' doc = do
    _uuid <- BSON.lookup "uuid" doc
    _name <- BSON.lookup "name" doc
    _level <- BSON.lookup "level" doc
    _accessibility <- BSON.lookup "accessibility" doc
    _packageId <- BSON.lookup "packageId" doc
    _selectedTagUuids <- BSON.lookup "selectedTagUuids" doc
    let _templateUuid = BSON.lookup "templateUuid" doc
    let _formatUuid = BSON.lookup "formatUuid" doc
    let _ownerUuid = BSON.lookup "ownerUuid" doc
    let _creatorUuid = BSON.lookup "creatorUuid" doc
    _replies <- BSON.lookup "replies" doc
    _labels <- BSON.lookup "labels" doc
    _createdAt <- BSON.lookup "createdAt" doc
    _updatedAt <- BSON.lookup "updatedAt" doc
    return Questionnaire {..}
