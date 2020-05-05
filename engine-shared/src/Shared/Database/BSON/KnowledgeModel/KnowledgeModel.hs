module Shared.Database.BSON.KnowledgeModel.KnowledgeModel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel where
  toBSON = toBSON'

instance FromBSON KnowledgeModel where
  fromBSON = fromBSON'

instance ToBSON KnowledgeModelEntities where
  toBSON = toBSON'

instance FromBSON KnowledgeModelEntities where
  fromBSON = fromBSON'

-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter where
  toBSON = toBSON'

instance FromBSON Chapter where
  fromBSON = fromBSON'

-- -------------------------
-- QUESTION ----------------
-- -------------------------
instance ToBSON Question where
  toBSON = toBSON'

instance FromBSON Question where
  fromBSON = fromBSON'

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
instance ToBSON Answer where
  toBSON = toBSON'

instance FromBSON Answer where
  fromBSON = fromBSON'

-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert where
  toBSON = toBSON'

instance FromBSON Expert where
  fromBSON = fromBSON'

-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance ToBSON Reference where
  toBSON = toBSON'

instance FromBSON Reference where
  fromBSON = fromBSON'

-- -------------------------
-- METRIC ------------------
instance ToBSON Metric where
  toBSON = toBSON'

instance FromBSON Metric where
  fromBSON = fromBSON'

instance ToBSON MetricMeasure where
  toBSON = toBSON'

instance FromBSON MetricMeasure where
  fromBSON = fromBSON'

-- -------------------------
-- TAG ---------------------
-- -------------------------
instance ToBSON Tag where
  toBSON = toBSON'

instance FromBSON Tag where
  fromBSON = fromBSON'

-- -------------------------
-- INTEGRATION -------------
-- -------------------------
instance ToBSON Integration where
  toBSON = toBSON'

instance FromBSON Integration where
  fromBSON = fromBSON'