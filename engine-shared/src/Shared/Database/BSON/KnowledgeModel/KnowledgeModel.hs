module Shared.Database.BSON.KnowledgeModel.KnowledgeModel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel


instance FromBSON KnowledgeModel


instance ToBSON KnowledgeModelEntities


instance FromBSON KnowledgeModelEntities


-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter


instance FromBSON Chapter


-- -------------------------
-- QUESTION ----------------
-- -------------------------
instance ToBSON Question


instance FromBSON Question


-- ------------------------------------------------
instance BSON.Val QuestionValueType where
  val StringQuestionValueType = BSON.String "StringQuestionValueType"
  val NumberQuestionValueType = BSON.String "NumberQuestionValueType"
  val DateQuestionValueType = BSON.String "DateQuestionValueType"
  val TextQuestionValueType = BSON.String "TextQuestionValueType"
  cast' (BSON.String "StringQuestionValueType") = Just StringQuestionValueType
  cast' (BSON.String "NumberQuestionValueType") = Just NumberQuestionValueType
  cast' (BSON.String "DateQuestionValueType") = Just DateQuestionValueType
  cast' (BSON.String "TextQuestionValueType") = Just TextQuestionValueType
  cast' _ = Nothing

-- -------------------------
-- ANSWER ------------------
-- -------------------------
instance ToBSON Answer


instance FromBSON Answer


-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert


instance FromBSON Expert


-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance ToBSON Reference


instance FromBSON Reference


-- -------------------------
-- METRIC ------------------
instance ToBSON Metric

instance FromBSON Metric

instance ToBSON MetricMeasure

instance FromBSON MetricMeasure

-- -------------------------
-- TAG ---------------------
-- -------------------------
instance ToBSON Tag


instance FromBSON Tag


-- -------------------------
-- INTEGRATION -------------
-- -------------------------
instance ToBSON Integration


instance FromBSON Integration
