module Shared.Database.Migration.Development.Event.Data.Events where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

a_km1 :: Event
a_km1 =
  AddKnowledgeModelEvent
    { _uuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
    , _parentUuid = U.nil
    , _entityUuid = km1WithoutChaptersAndTagsAndIntegrations ^. uuid
    , _name = DirectValue $ km1WithoutChaptersAndTagsAndIntegrations ^. name
    }

e_km1 :: Event
e_km1 =
  EditKnowledgeModelEvent
    { _uuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
    , _parentUuid = U.nil
    , _entityUuid = km1 ^. uuid
    , _name = ChangedValue $ km1Edited ^. name
    , _chapterUuids = ChangedValue $ km1Edited ^. chapterUuids
    , _tagUuids = ChangedValue $ km1Edited ^. tagUuids
    , _integrationUuids = ChangedValue $ km1Edited ^. integrationUuids
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: Event
a_km1_ch1 =
  AddChapterEvent
    { _uuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter1WithoutQuestions ^. uuid
    , _title = DirectValue $ chapter1WithoutQuestions ^. title
    , _text = DirectValue $ chapter1WithoutQuestions ^. text
    }

a_km1_ch2 :: Event
a_km1_ch2 =
  AddChapterEvent
    { _uuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter2WithoutQuestions ^. uuid
    , _title = DirectValue $ chapter2WithoutQuestions ^. title
    , _text = DirectValue $ chapter2WithoutQuestions ^. text
    }

a_km1_ch3 :: Event
a_km1_ch3 =
  AddChapterEvent
    { _uuid = fromJust $ U.fromString "6eaa2b47-711d-4187-98f8-fccdce94db9b"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter3 ^. uuid
    , _title = DirectValue $ chapter3 ^. title
    , _text = DirectValue $ chapter3 ^. text
    }

a_km1_ch4 :: Event
a_km1_ch4 =
  AddChapterEvent
    { _uuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter4WithoutQuestions ^. uuid
    , _title = DirectValue $ chapter4WithoutQuestions ^. title
    , _text = DirectValue $ chapter4WithoutQuestions ^. text
    }

e_km1_ch1 :: Event
e_km1_ch1 =
  EditChapterEvent
    { _uuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter1 ^. uuid
    , _eitle = ChangedValue $ chapter1Edited ^. title
    , _eext = ChangedValue $ chapter1Edited ^. text
    , _questionUuids = ChangedValue $ chapter1Edited ^. questionUuids
    }

e_km1_ch1_2 :: Event
e_km1_ch1_2 =
  EditChapterEvent
    { _uuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter1 ^. uuid
    , _eitle = ChangedValue $ "TWICE: " ++ chapter1Edited ^. title
    , _eext = ChangedValue $ chapter1Edited ^. text
    , _questionUuids = ChangedValue $ chapter1Edited ^. questionUuids
    }

d_km1_ch1 :: Event
d_km1_ch1 =
  DeleteChapterEvent
    { _uuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = chapter1 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: Event
a_km1_ch1_q1 =
  AddValueQuestionEvent
    { _uuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1 ^. uuid
    , _title = DirectValue $ question1 ^. title
    , _text = DirectValue $ question1 ^. text
    , _requiredLevel = DirectValue $ question1 ^. requiredLevel'
    , _tagUuids = DirectValue $ question1 ^. tagUuids
    , _valueType = DirectValue $ question1 ^. valueType'
    }

a_km1_ch1_q2 :: Event
a_km1_ch1_q2 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question2 ^. uuid
    , _title = DirectValue $ question2 ^. title
    , _text = DirectValue $ question2 ^. text
    , _requiredLevel = DirectValue $ question2 ^. requiredLevel'
    , _tagUuids = DirectValue $ question2 ^. tagUuids
    }

a_km1_ch1_q3 :: Event
a_km1_ch1_q3 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question3 ^. uuid
    , _title = DirectValue $ question3 ^. title
    , _text = DirectValue $ question3 ^. text
    , _requiredLevel = DirectValue $ question3 ^. requiredLevel'
    , _tagUuids = DirectValue $ question3 ^. tagUuids
    }

a_km1_ch2_q3 :: Event
a_km1_ch2_q3 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , _parentUuid = chapter2 ^. uuid
    , _entityUuid = question3 ^. uuid
    , _title = DirectValue $ question3 ^. title
    , _text = DirectValue $ question3 ^. text
    , _requiredLevel = DirectValue $ question3 ^. requiredLevel'
    , _tagUuids = DirectValue $ question3 ^. tagUuids
    }

a_km1_ch2_q4 :: Event
a_km1_ch2_q4 =
  AddListQuestionEvent
    { _uuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , _parentUuid = chapter2 ^. uuid
    , _entityUuid = question4 ^. uuid
    , _title = DirectValue $ question4 ^. title
    , _text = DirectValue $ question4 ^. text
    , _requiredLevel = DirectValue $ question4 ^. requiredLevel'
    , _tagUuids = DirectValue $ question4 ^. tagUuids
    }

a_km1_ch3_q9 :: Event
a_km1_ch3_q9 =
  AddIntegrationQuestionEvent
    { _uuid = fromJust $ U.fromString "51526318-2727-4113-993d-bae5d4abafcd"
    , _parentUuid = chapter3 ^. uuid
    , _entityUuid = question9 ^. uuid
    , _title = DirectValue $ question9 ^. title
    , _text = DirectValue $ question9 ^. text
    , _requiredLevel = DirectValue $ question9 ^. requiredLevel'
    , _tagUuids = DirectValue $ question9 ^. tagUuids
    , _integrationUuid = DirectValue $ question9 ^. integrationUuid'
    , _integrationProps = DirectValue $ question9 ^. integrationProps'
    }

a_km1_ch3_q10 :: Event
a_km1_ch3_q10 =
  AddIntegrationQuestionEvent
    { _uuid = fromJust $ U.fromString "e8531168-946d-4d95-a3b5-f092d32dee1a"
    , _parentUuid = chapter3 ^. uuid
    , _entityUuid = question10 ^. uuid
    , _title = DirectValue $ question10 ^. title
    , _text = DirectValue $ question10 ^. text
    , _requiredLevel = DirectValue $ question10 ^. requiredLevel'
    , _tagUuids = DirectValue $ question10 ^. tagUuids
    , _integrationUuid = DirectValue $ question10 ^. integrationUuid'
    , _integrationProps = DirectValue $ question10 ^. integrationProps'
    }


e_km1_ch1_q1 :: Event
e_km1_ch1_q1 =
  EditValueQuestionEvent
    { _uuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1Edited ^. uuid
    , _title = ChangedValue $ question1Edited ^. title
    , _text = NothingChanged
    , _requiredLevel = NothingChanged
    , _tagUuids = NothingChanged
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _valueType = NothingChanged
    }

e_km1_ch1_q1_type :: Event
e_km1_ch1_q1_type =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1WithNewType ^. uuid
    , _title = ChangedValue $ question1WithNewType ^. title
    , _text = NothingChanged
    , _requiredLevel = NothingChanged
    , _tagUuids = NothingChanged
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _answerUuids = ChangedValue $ question1WithNewType ^. answerUuids'
    }

e_km1_ch1_q2 :: Event
e_km1_ch1_q2 =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "1a01665b-e896-450d-b606-afc1dcca586b"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question2 ^. uuid
    , _title = ChangedValue $ question2Edited ^. title
    , _text = ChangedValue $ question2Edited ^. text
    , _requiredLevel = ChangedValue $ question2Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ question2Edited ^. tagUuids
    , _expertUuids = ChangedValue $ question2Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ question2Edited ^. referenceUuids'
    , _answerUuids = ChangedValue $ question2Edited ^. answerUuids'
    }

e_km1_ch1_q2_second_edit :: Event
e_km1_ch1_q2_second_edit =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question2 ^. uuid
    , _title = ChangedValue "New title"
    , _text = ChangedValue $ question2Edited ^. text
    , _requiredLevel = ChangedValue $ question2Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ question2Edited ^. tagUuids
    , _expertUuids = ChangedValue $ question2Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ question2Edited ^. referenceUuids'
    , _answerUuids = ChangedValue $ question2Edited ^. answerUuids'
    }

e_km1_ch1_q2_type :: Event
e_km1_ch1_q2_type =
  EditListQuestionEvent
    { _uuid = fromJust $ U.fromString "2727c225-78e5-4d5f-a093-cfaadb6ea663"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question2WithNewType ^. uuid
    , _title = ChangedValue $ question2WithNewType ^. title
    , _text = NothingChanged
    , _requiredLevel = NothingChanged
    , _tagUuids = NothingChanged
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _itemTemplateQuestionUuids = ChangedValue []
    }

e_km1_ch2_q4 :: Event
e_km1_ch2_q4 =
  EditListQuestionEvent
    { _uuid = fromJust $ U.fromString "7014c6de-a1c0-4c09-881a-c83c68a29de1"
    , _parentUuid = chapter2 ^. uuid
    , _entityUuid = question4Edited ^. uuid
    , _title = ChangedValue $ question4Edited ^. title
    , _text = ChangedValue $ question4Edited ^. text
    , _requiredLevel = ChangedValue $ question4Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ question4Edited ^. tagUuids
    , _expertUuids = ChangedValue $ question4Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ question4Edited ^. referenceUuids'
    , _itemTemplateQuestionUuids = ChangedValue $ question4Edited ^. itemTemplateQuestionUuids
    }

e_km1_ch2_q4_type :: Event
e_km1_ch2_q4_type =
  EditIntegrationQuestionEvent
    { _uuid = fromJust $ U.fromString "0f6f536c-aa1c-4d47-8cd7-46d611b43a56"
    , _parentUuid = chapter2 ^. uuid
    , _entityUuid = question4WithNewType ^. uuid
    , _title = ChangedValue $ question4WithNewType ^. title
    , _text = NothingChanged
    , _requiredLevel = NothingChanged
    , _tagUuids = NothingChanged
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _integrationUuid = ChangedValue $ question4WithNewType ^. integrationUuid'
    , _integrationProps = ChangedValue $ question4WithNewType ^. integrationProps'
    }

e_km1_ch3_q9 :: Event
e_km1_ch3_q9 =
  EditIntegrationQuestionEvent
    { _uuid = fromJust $ U.fromString "43779823-507b-41f1-8dce-7c5e0660db8f"
    , _parentUuid = chapter3 ^. uuid
    , _entityUuid = question9Edited ^. uuid
    , _title = ChangedValue $ question9Edited ^. title
    , _text = ChangedValue $ question9Edited ^. text
    , _requiredLevel = ChangedValue $ question9Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ question9Edited ^. tagUuids
    , _expertUuids = ChangedValue $ question9Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ question9Edited ^. referenceUuids'
    , _integrationUuid = ChangedValue $ question9Edited ^. integrationUuid'
    , _integrationProps = ChangedValue $ question9Edited ^. integrationProps'
    }

e_km1_ch3_q9_type :: Event
e_km1_ch3_q9_type =
  EditValueQuestionEvent
    { _uuid = fromJust $ U.fromString "91514dc3-29b1-469a-b0d9-5fc211df1c47"
    , _parentUuid = chapter3 ^. uuid
    , _entityUuid = question9WithNewType ^. uuid
    , _title = ChangedValue $ question9WithNewType ^. title
    , _text = NothingChanged
    , _requiredLevel = NothingChanged
    , _tagUuids = NothingChanged
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _valueType = ChangedValue $ question9WithNewType ^. valueType'
    }

d_km1_ch1_q1 :: Event
d_km1_ch1_q1 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1 ^. uuid
    }

d_km1_ch1_q1_2 :: Event
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1 ^. uuid
    }

d_km1_ch1_q2 :: Event
d_km1_ch1_q2 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "52a7a6ae-be37-4075-ac5c-a20858707a75"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question2 ^. uuid
    }

d_km1_ch1_q3 :: Event
d_km1_ch1_q3 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question3 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: Event
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerNo ^. uuid
    , _label = DirectValue $ q2_answerNo ^. label
    , _advice = DirectValue $ q2_answerNo ^. advice
    , _metricMeasures = DirectValue $ q2_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYes1 :: Event
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerYes ^. uuid
    , _label = DirectValue $ q2_answerYes ^. label
    , _advice = DirectValue $ q2_answerYes ^. advice
    , _metricMeasures = DirectValue $ q2_answerYes ^. metricMeasures
    }

a_km1_ch1_q2_aMaybe :: Event
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerMaybe ^. uuid
    , _label = DirectValue $ q2_answerMaybe ^. label
    , _advice = DirectValue $ q2_answerMaybe ^. advice
    , _metricMeasures = DirectValue $ q2_answerMaybe ^. metricMeasures
    }

a_km1_ch1_q2_aYes1_fuq1_aNo :: Event
a_km1_ch1_q2_aYes1_fuq1_aNo =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
    , _parentUuid = q2_aYes_fuQuestion1 ^. uuid
    , _entityUuid = q2_aYes_fuq1_answerNo ^. uuid
    , _label = DirectValue $ q2_aYes_fuq1_answerNo ^. label
    , _advice = DirectValue $ q2_aYes_fuq1_answerNo ^. advice
    , _metricMeasures = DirectValue $ q2_aYes_fuq1_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYesFu1 :: Event
a_km1_ch1_q2_aYesFu1 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
    , _parentUuid = q2_aYes_fuQuestion1 ^. uuid
    , _entityUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _label = DirectValue $ q2_aYes_fuq1_answerYes ^. label
    , _advice = DirectValue $ q2_aYes_fuq1_answerYes ^. advice
    , _metricMeasures = DirectValue $ q2_aYes_fuq1_answerYes ^. metricMeasures
    }

a_km1_ch1_q2_aNoFu2 :: Event
a_km1_ch1_q2_aNoFu2 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
    , _parentUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid
    , _label = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerNo ^. label
    , _advice = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerNo ^. advice
    , _metricMeasures = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYesFu2 :: Event
a_km1_ch1_q2_aYesFu2 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
    , _parentUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid
    , _label = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerYes ^. label
    , _advice = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerYes ^. advice
    , _metricMeasures = DirectValue $ q2_aYes_fuq1_aYes_fuq2_answerYes ^. metricMeasures
    }

a_km1_ch2_q3_aNo2 :: Event
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
    , _parentUuid = question3 ^. uuid
    , _entityUuid = q3_answerNo ^. uuid
    , _label = DirectValue $ q3_answerNo ^. label
    , _advice = DirectValue $ q3_answerNo ^. advice
    , _metricMeasures = DirectValue $ q3_answerNo ^. metricMeasures
    }

a_km1_ch2_q3_aYes2 :: Event
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
    , _parentUuid = question3 ^. uuid
    , _entityUuid = q3_answerYes ^. uuid
    , _label = DirectValue $ q3_answerYes ^. label
    , _advice = DirectValue $ q3_answerYes ^. advice
    , _metricMeasures = DirectValue $ q3_answerYes ^. metricMeasures
    }

a_km1_ch2_q4_it_q6_aNo :: Event
a_km1_ch2_q4_it_q6_aNo =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = q4_it1_q6_answerNo ^. uuid
    , _label = DirectValue $ q4_it1_q6_answerNo ^. label
    , _advice = DirectValue $ q4_it1_q6_answerNo ^. advice
    , _metricMeasures = DirectValue $ q4_it1_q6_answerNo ^. metricMeasures
    }

a_km1_ch2_q4_it_q6_aYes :: Event
a_km1_ch2_q4_it_q6_aYes =
  AddAnswerEvent
    { _uuid = fromJust $ U.fromString "c5c42f99-613b-4b6c-ae5e-af784f51c483"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = q4_it1_q6_answerYes ^. uuid
    , _label = DirectValue $ q4_it1_q6_answerYes ^. label
    , _advice = DirectValue $ q4_it1_q6_answerYes ^. advice
    , _metricMeasures = DirectValue $ q4_it1_q6_answerYes ^. metricMeasures
    }

e_km1_ch1_q2_aYes1 :: Event
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
    { _uuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerYes ^. uuid
    , _label = ChangedValue $ q2_answerYesEdited ^. label
    , _advice = ChangedValue $ q2_answerYesEdited ^. advice
    , _followUpUuids = ChangedValue $ q2_answerYesEdited ^. followUpUuids
    , _metricMeasures = ChangedValue $ q2_answerYesEdited ^. metricMeasures
    }

e_km1_ch1_q2_aYes1_2 :: Event
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
    { _uuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerYes ^. uuid
    , _label = ChangedValue $ q2_answerYesEdited ^. label
    , _advice = ChangedValue $ q2_answerYesEdited ^. advice
    , _followUpUuids = ChangedValue $ q2_answerYes ^. followUpUuids
    , _metricMeasures = ChangedValue $ q2_answerYes ^. metricMeasures
    }

d_km1_ch1_q2_aYes1 :: Event
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
    { _uuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerYes ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- AnswerItemTemplateQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch2_q4_it1_q5 :: Event
a_km1_ch2_q4_it1_q5 =
  AddListQuestionEvent
    { _uuid = fromJust $ U.fromString "5619d036-0130-47fa-9553-b73094eecd7e"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question5 ^. uuid
    , _title = DirectValue $ q4_it1_question5 ^. title
    , _text = DirectValue $ q4_it1_question5 ^. text
    , _requiredLevel = DirectValue $ q4_it1_question5 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_question5 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6 :: Event
a_km1_ch2_q4_it1_q6 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "5ac56741-b93a-42f5-9beb-f22100e4342d"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question6 ^. uuid
    , _title = DirectValue $ q4_it1_question6 ^. title
    , _text = DirectValue $ q4_it1_question6 ^. text
    , _requiredLevel = DirectValue $ q4_it1_question6 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_question6 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6_fuq4_q1 :: Event
a_km1_ch2_q4_it1_q6_fuq4_q1 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "55f46913-a953-4318-b72f-673e9f65fb2a"
    , _parentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _entityUuid = q4_it1_q6_aYes_fuq4_it_question1 ^. uuid
    , _title = DirectValue $ q4_it1_q6_aYes_fuq4_it_question1 ^. title
    , _text = DirectValue $ q4_it1_q6_aYes_fuq4_it_question1 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q6_aYes_fuq4_it_question1 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q6_aYes_fuq4_it_question1 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6_fuq4_q2 :: Event
a_km1_ch2_q4_it1_q6_fuq4_q2 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
    , _parentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _entityUuid = q4_it1_q6_aYes_fuq4_it_question2 ^. uuid
    , _title = DirectValue $ q4_it1_q6_aYes_fuq4_it_question2 ^. title
    , _text = DirectValue $ q4_it1_q6_aYes_fuq4_it_question2 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q6_aYes_fuq4_it_question2 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q6_aYes_fuq4_it_question2 ^. tagUuids
    }

a_km1_ch2_q4_it1_q7 :: Event
a_km1_ch2_q4_it1_q7 =
  AddValueQuestionEvent
    { _uuid = fromJust $ U.fromString "cf839365-91d0-427a-bb99-89de1a125929"
    , _parentUuid = q4_it1_question5 ^. uuid
    , _entityUuid = q4_it1_q5_it2_question7 ^. uuid
    , _title = DirectValue $ q4_it1_q5_it2_question7 ^. title
    , _text = DirectValue $ q4_it1_q5_it2_question7 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q5_it2_question7 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q5_it2_question7 ^. tagUuids
    , _valueType = DirectValue $ q4_it1_q5_it2_question7 ^. valueType'
    }

a_km1_ch2_q4_it1_q8 :: Event
a_km1_ch2_q4_it1_q8 =
  AddValueQuestionEvent
    { _uuid = fromJust $ U.fromString "3536a56f-d19c-4aff-ada1-ef7b3a60389d"
    , _parentUuid = q4_it1_question5 ^. uuid
    , _entityUuid = q4_it1_q5_it2_question8 ^. uuid
    , _title = DirectValue $ q4_it1_q5_it2_question8 ^. title
    , _text = DirectValue $ q4_it1_q5_it2_question8 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q5_it2_question8 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q5_it2_question8 ^. tagUuids
    , _valueType = DirectValue $ q4_it1_q5_it2_question8 ^. valueType'
    }

e_km1_ch2_q4_it1_q5 :: Event
e_km1_ch2_q4_it1_q5 =
  EditListQuestionEvent
    { _uuid = fromJust $ U.fromString "17f8e9d4-7299-4c88-aba1-0a7b133aa8f3"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question5Edited ^. uuid
    , _title = ChangedValue $ q4_it1_question5Edited ^. title
    , _text = ChangedValue $ q4_it1_question5Edited ^. text
    , _requiredLevel = ChangedValue $ q4_it1_question5Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ q4_it1_question5Edited ^. tagUuids
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _itemTemplateQuestionUuids =
        ChangedValue [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
    }

e_km1_ch2_q4_it1_q6 :: Event
e_km1_ch2_q4_it1_q6 =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "f5c5ccfd-619b-4110-807a-39ede6d31cae"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question6Edited ^. uuid
    , _title = ChangedValue $ q4_it1_question6Edited ^. title
    , _text = ChangedValue $ q4_it1_question6Edited ^. text
    , _requiredLevel = ChangedValue $ q4_it1_question6Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ q4_it1_question6Edited ^. tagUuids
    , _expertUuids = ChangedValue $ q4_it1_question6Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ q4_it1_question6Edited ^. referenceUuids'
    , _answerUuids = ChangedValue $ q4_it1_question6Edited ^. answerUuids'
    }

d_km1_ch2_q4_it1_q5 :: Event
d_km1_ch2_q4_it1_q5 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "424d19cb-a79f-4da0-b7f6-33363c32b7fd"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question5 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- FollowUpQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: Event
a_km1_ch1_ansYes1_fuq1 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
    , _parentUuid = q2_answerYes ^. uuid
    , _entityUuid = q2_aYes_fuQuestion1 ^. uuid
    , _title = DirectValue $ q2_aYes_fuQuestion1 ^. title
    , _text = DirectValue $ q2_aYes_fuQuestion1 ^. text
    , _requiredLevel = DirectValue $ q2_aYes_fuQuestion1 ^. requiredLevel'
    , _tagUuids = DirectValue $ q2_aYes_fuQuestion1 ^. tagUuids
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 :: Event
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
    , _parentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _title = DirectValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. title
    , _text = DirectValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. text
    , _requiredLevel = DirectValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. requiredLevel'
    , _tagUuids = DirectValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. tagUuids
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 :: Event
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 =
  AddOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
    , _parentUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid
    , _entityUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. uuid
    , _title = DirectValue $ q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. title
    , _text = DirectValue $ q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. text
    , _requiredLevel = DirectValue $ q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. requiredLevel'
    , _tagUuids = DirectValue $ q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. tagUuids
    }

a_km1_ch2_ansYes6_fuq4 :: Event
a_km1_ch2_ansYes6_fuq4 =
  AddListQuestionEvent
    { _uuid = fromJust $ U.fromString "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
    , _parentUuid = q4_it1_q6_answerYes ^. uuid
    , _entityUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _title = DirectValue $ q4_it1_q6_aYes_followUpQuestion4 ^. title
    , _text = DirectValue $ q4_it1_q6_aYes_followUpQuestion4 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q6_aYes_followUpQuestion4 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q6_aYes_followUpQuestion4 ^. tagUuids
    }

a_km1_ch2_ansYes6_fuq5 :: Event
a_km1_ch2_ansYes6_fuq5 =
  AddIntegrationQuestionEvent
    { _uuid = fromJust $ U.fromString "11872ad2-0d3d-4ab6-b81c-17d234bab6ba"
    , _parentUuid = q4_it1_q6_answerYes ^. uuid
    , _entityUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _title = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _text = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _requiredLevel = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. requiredLevel'
    , _tagUuids = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _integrationUuid = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. integrationUuid'
    , _integrationProps = DirectValue $ q4_it1_q6_aYes_followUpQuestion5 ^. integrationProps'
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: Event
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _parentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
    , _text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
    , _requiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
    , _expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. expertUuids'
    , _referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. referenceUuids'
    , _answerUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. answerUuids'
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: Event
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditOptionsQuestionEvent
    { _uuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _parentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
    , _text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
    , _requiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
    , _expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. expertUuids'
    , _referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2 ^. referenceUuids'
    , _answerUuids =
        ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
    }

e_km1_ch2_ansMaybe6_fuq4 :: Event
e_km1_ch2_ansMaybe6_fuq4 =
  EditListQuestionEvent
    { _uuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _parentUuid = q4_it1_q6_answerNo ^. uuid
    , _entityUuid = q4_it1_q6_aYes_followUpQuestion4Edited ^. uuid
    , _title = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. title
    , _text = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. text
    , _requiredLevel = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. requiredLevel'
    , _tagUuids = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. tagUuids
    , _expertUuids = NothingChanged
    , _referenceUuids = NothingChanged
    , _itemTemplateQuestionUuids =
        ChangedValue [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
    }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: Event
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteQuestionEvent
    { _uuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
    , _parentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _entityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eAlbert :: Event
a_km1_ch1_q2_eAlbert =
  AddExpertEvent
    { _uuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eAlbert ^. uuid
    , _name = DirectValue $ km1_ch1_q2_eAlbert ^. name
    , _email = DirectValue $ km1_ch1_q2_eAlbert ^. email
    }

a_km1_ch2_q6_eAlbert :: Event
a_km1_ch2_q6_eAlbert =
  AddExpertEvent
    { _uuid = fromJust $ U.fromString "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = km1_ch2_q6_eAlbert ^. uuid
    , _name = DirectValue $ km1_ch2_q6_eAlbert ^. name
    , _email = DirectValue $ km1_ch2_q6_eAlbert ^. email
    }

a_km1_ch1_q2_eNikola :: Event
a_km1_ch1_q2_eNikola =
  AddExpertEvent
    { _uuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eNikola ^. uuid
    , _name = DirectValue $ km1_ch1_q2_eNikola ^. name
    , _email = DirectValue $ km1_ch1_q2_eNikola ^. email
    }

a_km1_ch2_q6_eNikola :: Event
a_km1_ch2_q6_eNikola =
  AddExpertEvent
    { _uuid = fromJust $ U.fromString "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = km1_ch2_q6_eNikola ^. uuid
    , _name = DirectValue $ km1_ch2_q6_eNikola ^. name
    , _email = DirectValue $ km1_ch2_q6_eNikola ^. email
    }

a_km1_ch1_q2_eIsaac :: Event
a_km1_ch1_q2_eIsaac =
  AddExpertEvent
    { _uuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eIsaac ^. uuid
    , _name = DirectValue $ km1_ch1_q2_eIsaac ^. name
    , _email = DirectValue $ km1_ch1_q2_eIsaac ^. email
    }

e_km1_ch1_q2_eAlbert :: Event
e_km1_ch1_q2_eAlbert =
  EditExpertEvent
    { _uuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eAlbertEdited ^. uuid
    , _name = ChangedValue $ km1_ch1_q2_eAlbertEdited ^. name
    , _email = ChangedValue $ km1_ch1_q2_eAlbertEdited ^. email
    }

d_km1_ch1_q2_eNikola :: Event
d_km1_ch1_q2_eNikola =
  DeleteExpertEvent
    { _uuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eNikola ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: Event
a_km1_ch1_q2_rCh1 =
  AddResourcePageReferenceEvent
    { _uuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r1 ^. uuid
    , _shortUuid = DirectValue $ km1_ch1_q2_r1 ^. shortUuid
    }

a_km1_ch2_q6_rCh1 :: Event
a_km1_ch2_q6_rCh1 =
  AddResourcePageReferenceEvent
    { _uuid = fromJust $ U.fromString "a3f6ee9a-803f-4911-9566-734a6358913a"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = km1_ch2_q6_r1 ^. uuid
    , _shortUuid = DirectValue $ km1_ch2_q6_r1 ^. shortUuid
    }

a_km1_ch1_q2_rCh2 :: Event
a_km1_ch1_q2_rCh2 =
  AddURLReferenceEvent
    { _uuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r2 ^. uuid
    , _url = DirectValue $ km1_ch1_q2_r2 ^. url
    , _label = DirectValue $ km1_ch1_q2_r2 ^. label
    }

a_km1_ch2_q6_rCh2 :: Event
a_km1_ch2_q6_rCh2 =
  AddURLReferenceEvent
    { _uuid = fromJust $ U.fromString "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
    , _parentUuid = q4_it1_question6 ^. uuid
    , _entityUuid = km1_ch2_q6_r2 ^. uuid
    , _url = DirectValue $ km1_ch2_q6_r2 ^. url
    , _label = DirectValue $ km1_ch2_q6_r2 ^. label
    }

a_km1_ch1_q2_rCh3 :: Event
a_km1_ch1_q2_rCh3 =
  AddCrossReferenceEvent
    { _uuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r3 ^. uuid
    , _targetUuid = DirectValue $ km1_ch1_q2_r3 ^. targetUuid'
    , _label = DirectValue $ km1_ch1_q2_r3 ^. description
    }

e_km1_ch1_q2_rCh1 :: Event
e_km1_ch1_q2_rCh1 =
  EditResourcePageReferenceEvent
    { _uuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r1Edited ^. uuid
    , _shortUuid = ChangedValue $ km1_ch1_q2_r1Edited ^. shortUuid
    }

e_km1_ch1_q2_rCh1_type :: Event
e_km1_ch1_q2_rCh1_type =
  EditURLReferenceEvent
    { _uuid = fromJust $ U.fromString "4e1058cf-9044-42a0-901c-816bd6847b17"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r1WithNewType ^. uuid
    , _url = ChangedValue $ km1_ch1_q2_r1WithNewType ^. url
    , _label = ChangedValue $ km1_ch1_q2_r1WithNewType ^. label
    }

e_km1_ch1_q2_rCh2 :: Event
e_km1_ch1_q2_rCh2 =
  EditURLReferenceEvent
    { _uuid = fromJust $ U.fromString "f96588ae-1657-406e-9810-1d00f5e24a96"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r2Edited ^. uuid
    , _url = ChangedValue $ km1_ch1_q2_r2Edited ^. url
    , _label = ChangedValue $ km1_ch1_q2_r2Edited ^. label
    }

e_km1_ch1_q2_rCh2_type :: Event
e_km1_ch1_q2_rCh2_type =
  EditCrossReferenceEvent
    { _uuid = fromJust $ U.fromString "e0a19e9d-fb36-47b3-bc23-f752f7403937"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r2WithNewType ^. uuid
    , _targetUuid = ChangedValue $ km1_ch1_q2_r2WithNewType ^. targetUuid'
    , _label = ChangedValue $ km1_ch1_q2_r2WithNewType ^. description
    }

e_km1_ch1_q2_rCh3 :: Event
e_km1_ch1_q2_rCh3 =
  EditCrossReferenceEvent
    { _uuid = fromJust $ U.fromString "d3a7b6a6-9e87-4308-a103-88245537c26e"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r3Edited ^. uuid
    , _targetUuid = ChangedValue $ km1_ch1_q2_r3Edited ^. targetUuid'
    , _label = ChangedValue $ km1_ch1_q2_r3Edited ^. description
    }

e_km1_ch1_q2_rCh3_type :: Event
e_km1_ch1_q2_rCh3_type =
  EditResourcePageReferenceEvent
    { _uuid = fromJust $ U.fromString "f8528e3b-4904-4ad8-87b8-809d7e40c087"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r3WithNewType ^. uuid
    , _shortUuid = ChangedValue $ km1_ch1_q2_r3WithNewType ^. shortUuid
    }

d_km1_ch1_q2_rCh2 :: Event
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
    { _uuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r2 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_tds :: Event
a_km1_tds =
  AddTagEvent
    { _uuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = tagDataScience ^. uuid
    , _name = DirectValue $ tagDataScience ^. name
    , _description = DirectValue $ tagDataScience ^. description
    , _color = DirectValue $ tagDataScience ^. color
    }

a_km1_tbi :: Event
a_km1_tbi =
  AddTagEvent
    { _uuid = fromJust $ U.fromString "b6b0e53c-5702-403c-950c-e04960e09e73"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = tagBioInformatic ^. uuid
    , _name = DirectValue $ tagBioInformatic ^. name
    , _description = DirectValue $ tagBioInformatic ^. description
    , _color = DirectValue $ tagBioInformatic ^. color
    }

e_km1_tds :: Event
e_km1_tds =
  EditTagEvent
    { _uuid = fromJust $ U.fromString "f68f764b-48d1-4b30-8d53-48cfa2752801"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = tagDataScienceEdited ^. uuid
    , _name = ChangedValue $ tagDataScienceEdited ^. name
    , _description = ChangedValue $ tagDataScienceEdited ^. description
    , _color = ChangedValue $ tagDataScienceEdited ^. color
    }

d_km1_tds :: Event
d_km1_tds =
  DeleteTagEvent
    { _uuid = fromJust $ U.fromString "969d00c2-062d-4763-a372-536d486c532f"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = tagDataScience ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_iop :: Event
a_km1_iop =
  AddIntegrationEvent
    { _uuid = fromJust $ U.fromString "3f94cb01-6f92-4eb6-975b-385c02b831bc"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = ontologyPortal ^. uuid
    , _iId = DirectValue $ ontologyPortal ^. iId
    , _name = DirectValue $ ontologyPortal ^. name
    , _props = DirectValue $ ontologyPortal ^. props
    , _logo = DirectValue $ ontologyPortal ^. logo
    , _requestMethod = DirectValue $ ontologyPortal ^. requestMethod
    , _requestUrl = DirectValue $ ontologyPortal ^. requestUrl
    , _requestHeaders = DirectValue $ ontologyPortal ^. requestHeaders
    , _requestBody = DirectValue $ ontologyPortal ^. requestBody
    , _responseListField = DirectValue $ ontologyPortal ^. responseListField
    , _responseIdField = DirectValue $ ontologyPortal ^. responseIdField
    , _responseNameField = DirectValue $ ontologyPortal ^. responseNameField
    , _itemUrl = DirectValue $ ontologyPortal ^. itemUrl
    }

a_km1_ibp :: Event
a_km1_ibp =
  AddIntegrationEvent
    { _uuid = fromJust $ U.fromString "5c47b31c-84d0-4792-99ce-09154642105d"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = bioPortal ^. uuid
    , _iId = DirectValue $ bioPortal ^. iId
    , _name = DirectValue $ bioPortal ^. name
    , _props = DirectValue $ bioPortal ^. props
    , _logo = DirectValue $ bioPortal ^. logo
    , _requestMethod = DirectValue $ bioPortal ^. requestMethod
    , _requestUrl = DirectValue $ bioPortal ^. requestUrl
    , _requestHeaders = DirectValue $ bioPortal ^. requestHeaders
    , _requestBody = DirectValue $ bioPortal ^. requestBody
    , _responseListField = DirectValue $ bioPortal ^. responseListField
    , _responseIdField = DirectValue $ bioPortal ^. responseIdField
    , _responseNameField = DirectValue $ bioPortal ^. responseNameField
    , _itemUrl = DirectValue $ bioPortal ^. itemUrl
    }

e_km1_iop :: Event
e_km1_iop =
  EditIntegrationEvent
    { _uuid = fromJust $ U.fromString "3456a254-c5bc-4c0e-8ff9-f5e080765a71"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = ontologyPortalEdited ^. uuid
    , _iId = ChangedValue $ ontologyPortalEdited ^. iId
    , _name = ChangedValue $ ontologyPortalEdited ^. name
    , _props = ChangedValue $ ontologyPortalEdited ^. props
    , _logo = ChangedValue $ ontologyPortalEdited ^. logo
    , _requestMethod = ChangedValue $ ontologyPortalEdited ^. requestMethod
    , _requestUrl = ChangedValue $ ontologyPortalEdited ^. requestUrl
    , _requestHeaders = ChangedValue $ ontologyPortalEdited ^. requestHeaders
    , _requestBody = ChangedValue $ ontologyPortalEdited ^. requestBody
    , _responseListField = ChangedValue $ ontologyPortalEdited ^. responseListField
    , _responseIdField = ChangedValue $ ontologyPortalEdited ^. responseIdField
    , _responseNameField = ChangedValue $ ontologyPortalEdited ^. responseNameField
    , _itemUrl = ChangedValue $ ontologyPortalEdited ^. itemUrl
    }

d_km1_iop :: Event
d_km1_iop =
  DeleteIntegrationEvent
    { _uuid = fromJust $ U.fromString "d211d46f-5358-497a-92a0-e0bde08ce3d3"
    , _parentUuid = km1 ^. uuid
    , _entityUuid = ontologyPortal ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
m_km1_ch1_q1__to_ch2 :: Event
m_km1_ch1_q1__to_ch2 =
  MoveQuestionEvent
    { _uuid = fromJust $ U.fromString "f13a1d1b-5cb6-458a-ad99-cafe3912aa1d"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1 ^. uuid
    , _targetUuid = DirectValue $ chapter2 ^. uuid
    }

m_km1_ch1_q1__to_ch2_q3_aNo :: Event
m_km1_ch1_q1__to_ch2_q3_aNo =
  MoveQuestionEvent
    { _uuid = fromJust $ U.fromString "3bf501a1-cbc2-4b94-9b17-d23f0bad7fc9"
    , _parentUuid = chapter1 ^. uuid
    , _entityUuid = question1 ^. uuid
    , _targetUuid = DirectValue $ q3_answerNo ^. uuid
    }

m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo :: Event
m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo =
  MoveQuestionEvent
    { _uuid = fromJust $ U.fromString "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , _parentUuid = question4 ^. uuid
    , _entityUuid = q4_it1_question5 ^. uuid
    , _targetUuid = DirectValue $ q4_it1_q6_answerNo ^. uuid
    }

m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 :: Event
m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 =
  MoveQuestionEvent
    { _uuid = fromJust $ U.fromString "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , _parentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _entityUuid = q4_it1_q6_aYes_fuq4_it_question1 ^. uuid
    , _targetUuid = DirectValue $ question4 ^. uuid
    }

m_km1_ch1_q2_aYes__to_ch2_q3 :: Event
m_km1_ch1_q2_aYes__to_ch2_q3 =
  MoveAnswerEvent
    { _uuid = fromJust $ U.fromString "b660447a-ddbd-482a-9610-68dfca6a25fd"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = q2_answerYes ^. uuid
    , _targetUuid = DirectValue $ question3 ^. uuid
    }

m_km1_ch1_q2_eAlbert__to_ch2_q3 :: Event
m_km1_ch1_q2_eAlbert__to_ch2_q3 =
  MoveExpertEvent
    { _uuid = fromJust $ U.fromString "35b18cb0-912f-4c76-9f80-b6bfc6479c7c"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_eAlbert ^. uuid
    , _targetUuid = DirectValue $ question3 ^. uuid
    }

m_km1_ch1_q2_r1__to_ch2_q3 :: Event
m_km1_ch1_q2_r1__to_ch2_q3 =
  MoveReferenceEvent
    { _uuid = fromJust $ U.fromString "1cc9ad2b-22bc-4806-902e-49b46ccc14d5"
    , _parentUuid = question2 ^. uuid
    , _entityUuid = km1_ch1_q2_r1 ^. uuid
    , _targetUuid = DirectValue $ question3 ^. uuid
    }
