module Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions where

import Control.Lens
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations as FI
import Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags as FT
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
q2_answerNo :: Answer
q2_answerNo =
  Answer
    { _uuid = fromJust $ U.fromString "33da0831-11dd-4faa-b754-41ed98dedcb5"
    , _label = "No"
    , _advice = Just "Super long advice"
    , _followUpUuids = []
    , _metricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q3_answerNo :: Answer
q3_answerNo =
  Answer
    { _uuid = fromJust $ U.fromString "12711c8c-193a-4baf-a071-53f2d3990083"
    , _label = "No"
    , _advice = Just "Super long advice"
    , _followUpUuids = []
    , _metricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q4_it1_q6_answerNo :: Answer
q4_it1_q6_answerNo =
  Answer
    { _uuid = fromJust $ U.fromString "a093c2c3-123c-42ee-9667-13af14b6249e"
    , _label = "No"
    , _advice = Just "Great advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

q2_aYes_fuq1_answerNo :: Answer
q2_aYes_fuq1_answerNo =
  Answer
    { _uuid = fromJust $ U.fromString "8ebf2494-80c7-4dbb-a4a1-a14d3387abc0"
    , _label = "No"
    , _advice = Just "Super long advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerNo :: Answer
q2_aYes_fuq1_aYes_fuq2_answerNo =
  Answer
    { _uuid = fromJust $ U.fromString "891ebfe2-27df-433c-af83-03bb26fa2764"
    , _label = "No"
    , _advice = Just "Super long advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

q2_answerYes :: Answer
q2_answerYes =
  Answer
    { _uuid = fromJust $ U.fromString "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
    , _label = "Yes"
    , _advice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , _followUpUuids = [q2_aYes_fuQuestion1 ^. uuid]
    , _metricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q2_answerYesEdited :: Answer
q2_answerYesEdited =
  Answer
    { _uuid = q2_answerYes ^. uuid
    , _label = "EDITED: Yes"
    , _advice = Just "EDITED: Short advice"
    , _followUpUuids = []
    , _metricMeasures = [metricMeasureI1, metricMeasureR1, metricMeasureG1]
    }

q2_answerYesPlain :: Answer
q2_answerYesPlain =
  Answer
    { _uuid = fromJust $ U.fromString "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
    , _label = "Yes"
    , _advice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , _followUpUuids = []
    , _metricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q3_answerYes :: Answer
q3_answerYes =
  Answer
    { _uuid = fromJust $ U.fromString "28d49dbe-4180-49c9-80b2-397e9ea27c77"
    , _label = "Yes"
    , _advice = Just "Short advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

q2_aYes_fuq1_answerYes :: Answer
q2_aYes_fuq1_answerYes =
  Answer
    { _uuid = fromJust $ U.fromString "4d164317-d900-460c-8582-8c80e6d66dcd"
    , _label = "Yes"
    , _advice = Just "Short advice"
    , _followUpUuids = [q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid]
    , _metricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerYes :: Answer
q2_aYes_fuq1_aYes_fuq2_answerYes =
  Answer
    { _uuid = fromJust $ U.fromString "b6b40918-a9b7-4d2d-bacb-9f9aa5683efe"
    , _label = "Yes"
    , _advice = Just "Short advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

q4_it1_q6_answerYes :: Answer
q4_it1_q6_answerYes =
  Answer
    { _uuid = fromJust $ U.fromString "16f20d73-b335-47d8-8d35-157e8c3cd009"
    , _label = "Yes"
    , _advice = Just "Short advice"
    , _followUpUuids = [q4_it1_q6_aYes_followUpQuestion4 ^. uuid, q4_it1_q6_aYes_followUpQuestion5 ^. uuid]
    , _metricMeasures = []
    }

q2_answerMaybe :: Answer
q2_answerMaybe =
  Answer
    { _uuid = fromJust $ U.fromString "1f172f5e-3d66-4a1c-a785-85ba02fcf72a"
    , _label = "Maybe"
    , _advice = Just "Great advice"
    , _followUpUuids = []
    , _metricMeasures = []
    }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
q2_aYes_fuQuestion1:: Question
q2_aYes_fuQuestion1 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
    , _title = "First Follow-Up Question"
    , _text = Just "Maybe there will be some description"
    , _requiredLevel = Just 2
    , _tagUuids = [FT.tagDataScience ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = [q2_aYes_fuq1_answerNo ^. uuid, q2_aYes_fuq1_answerYes ^. uuid]
    }

q2_aYes_fuQuestion1Plain:: Question
q2_aYes_fuQuestion1Plain =
  OptionsQuestion
    { _uuid = q2_aYes_fuQuestion1 ^. uuid
    , _title = "Fourth Question"
    , _text = Just "Just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

-- -----------------------------------------------------------------------------
q2_aYes_fuq1_aYes_fuQuestion2:: Question
q2_aYes_fuq1_aYes_fuQuestion2 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "393eb40a-27bd-4156-9b2d-c4e8c582cca8"
    , _title = "Second Follow-Up Question"
    , _text = Just "Again just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid]
    }

q2_aYes_fuq1_aYes_fuQuestion2Edited:: Question
q2_aYes_fuq1_aYes_fuQuestion2Edited =
  OptionsQuestion
    { _uuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _title = "EDITED: Second Follow-Up Question"
    , _text = Just "EDITED: Again just follow"
    , _requiredLevel = Just 1
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
    }

-- -----------------------------------------------------------------------------
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3:: Question
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "70b6a446-bd35-4d5e-8995-78a94a69da83"
    , _title = "Third Follow-Up Question"
    , _text = Just "Again and again just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

-- -----------------------------------------------------------------------------
q4_it1_q6_aYes_followUpQuestion4:: Question
q4_it1_q6_aYes_followUpQuestion4 =
  ListQuestion
    { _uuid = fromJust $ U.fromString "cd98f76a-a430-4bd6-ba63-eb4c3c5c8c7e"
    , _title = "Fourth Follow-Up Question"
    , _text = Just "Again and again just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _itemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question1 ^. uuid, q4_it1_q6_aYes_fuq4_it_question2 ^. uuid]
    }

q4_it1_q6_aYes_followUpQuestion4Edited:: Question
q4_it1_q6_aYes_followUpQuestion4Edited =
  ListQuestion
    { _uuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _title = "EDITED: Third Follow-Up Question"
    , _text = Just "EDITED: Again and again just follow"
    , _requiredLevel = Just 1
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _itemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
    }

q4_it1_q6_aYes_fuq4_it_question1:: Question
q4_it1_q6_aYes_fuq4_it_question1 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "e5a3e1b2-077a-405f-b35c-3bffded63140"
    , _title = "Sub question 1 of Follow-Up Question 4"
    , _text = Just "Again and again just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

q4_it1_q6_aYes_fuq4_it_question2:: Question
q4_it1_q6_aYes_fuq4_it_question2 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "7f2e3fe5-b8b6-4b5a-812d-c5c1c704b3d9"
    , _title = "Sub question 2 of Follow-Up Question 4"
    , _text = Just "Again and again just follow"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

q4_it1_q6_aYes_followUpQuestion5:: Question
q4_it1_q6_aYes_followUpQuestion5 =
  IntegrationQuestion
    { _uuid = fromJust $ U.fromString "82f9a83a-88c8-439b-8cf8-8a028d5cce7d"
    , _title = "Fifth Follow-Up Question"
    , _text = Just "Some non-funny description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _integrationUuid = FI.ontologyPortal ^. uuid
    , _integrationProps = Map.fromList [("domain", "biology"), ("country", "be")]
    }

q4_it1_q6_aYes_fuq5PropsEdited:: Question
q4_it1_q6_aYes_fuq5PropsEdited =
  IntegrationQuestion
    { _uuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _title = q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _text = q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _requiredLevel = q4_it1_q6_aYes_followUpQuestion5 ^. requiredLevel'
    , _tagUuids = q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _referenceUuids = q4_it1_q6_aYes_followUpQuestion5 ^. referenceUuids
    , _expertUuids = q4_it1_q6_aYes_followUpQuestion5 ^. expertUuids
    , _integrationUuid = q4_it1_q6_aYes_followUpQuestion5 ^. integrationUuid'
    , _integrationProps = Map.fromList [("domain", "biology"), ("language", "")]
    }

q4_it1_q6_aYes_fuq5ConvertedToValue:: Question
q4_it1_q6_aYes_fuq5ConvertedToValue =
  ValueQuestion
    { _uuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _title = q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _text = q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _requiredLevel = q4_it1_q6_aYes_followUpQuestion5 ^. requiredLevel'
    , _tagUuids = q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _referenceUuids = q4_it1_q6_aYes_followUpQuestion5 ^. referenceUuids
    , _expertUuids = q4_it1_q6_aYes_followUpQuestion5 ^. expertUuids
    , _valueType = StringQuestionValueType
    }
