module Shared.Database.Migration.Development.KnowledgeModel.Data.Questions where

import Control.Lens
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

question1 :: Question
question1 =
  ValueQuestion
    { _uuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
    , _title = "First Question"
    , _text = Just "Here is a description of question"
    , _requiredLevel = Just 1
    , _tagUuids = [tagDataScience ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _valueType = StringQuestionValueType
    }

question1Edited :: Question
question1Edited =
  ValueQuestion
    { _uuid = question1 ^. uuid
    , _title = "EDITED: " ++ question1 ^. title
    , _text = question1 ^. text
    , _requiredLevel = question1 ^. requiredLevel'
    , _tagUuids = question1 ^. tagUuids
    , _referenceUuids = question1 ^. referenceUuids
    , _expertUuids = question1 ^. expertUuids
    , _valueType = question1 ^. valueType'
    }

question1WithNewType :: Question
question1WithNewType =
  OptionsQuestion
    { _uuid = question1 ^. uuid
    , _title = "EDITED: " ++ question1 ^. title
    , _text = question1 ^. text
    , _requiredLevel = question1 ^. requiredLevel'
    , _tagUuids = question1 ^. tagUuids
    , _referenceUuids = question1 ^. referenceUuids
    , _expertUuids = question1 ^. expertUuids
    , _answerUuids = []
    }

-- -----------------------------------
question2 :: Question
question2 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "549d4140-d3e7-4cda-a373-7af8abc6325c"
    , _title = "Is there any pre-existing data?"
    , _text =
        Just "Are there any data sets available in the world that are relevant to your planned research?"
    , _requiredLevel = Just 2
    , _tagUuids = [tagBioInformatic ^. uuid]
    , _referenceUuids = [km1_ch1_q2_r1 ^. uuid, km1_ch1_q2_r2 ^. uuid]
    , _expertUuids = [km1_ch1_q2_eAlbert ^. uuid, km1_ch1_q2_eNikola ^. uuid]
    , _answerUuids = [q2_answerNo ^. uuid, q2_answerYes ^. uuid]
    }

question2Edited :: Question
question2Edited =
  OptionsQuestion
    { _uuid = question2 ^. uuid
    , _title = "EDITED: Second Question"
    , _text = Just "EDITED: Some long description"
    , _requiredLevel = Just 3
    , _tagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _referenceUuids = [km1_ch1_q2_r2 ^. uuid, km1_ch1_q2_r1 ^. uuid]
    , _expertUuids = [km1_ch1_q2_eNikola ^. uuid, km1_ch1_q2_eAlbert ^. uuid]
    , _answerUuids = [q2_answerYes ^. uuid, q2_answerNo ^. uuid]
    }

question2WithNewType :: Question
question2WithNewType =
  ListQuestion
    { _uuid = question2 ^. uuid
    , _title = "EDITED: " ++ question2 ^. title
    , _text = question2 ^. text
    , _requiredLevel = question2 ^. requiredLevel'
    , _tagUuids = question2 ^. tagUuids
    , _referenceUuids = question2 ^. referenceUuids
    , _expertUuids = question2 ^. expertUuids
    , _itemTemplateQuestionUuids = []
    }

question2Plain :: Question
question2Plain =
  OptionsQuestion
    { _uuid = question2 ^. uuid
    , _title = question2 ^. title
    , _text = question2 ^. text
    , _requiredLevel = question2 ^. requiredLevel'
    , _tagUuids = question2 ^. tagUuids
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

question3 :: Question
question3 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "b12d5939-2bd5-42b3-af09-a189480014d9"
    , _title = "Third Question"
    , _text = Just "Some long description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = [q3_answerNo ^. uuid, q3_answerYes ^. uuid]
    }

question3Plain :: Question
question3Plain =
  OptionsQuestion
    { _uuid = question3 ^. uuid
    , _title = "Third Question"
    , _text = Just "Some long description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _answerUuids = []
    }

question4 :: Question
question4 =
  ListQuestion
    { _uuid = fromJust $ U.fromString "5c995368-b8dc-49e2-9b38-b79d4eb2779b"
    , _title = "Fourth Question"
    , _text = Just "Some nice description"
    , _requiredLevel = Nothing
    , _tagUuids = [tagBioInformatic ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _itemTemplateQuestionUuids = [q4_it1_question5 ^. uuid, q4_it1_question6 ^. uuid]
    }

question4Edited :: Question
question4Edited =
  ListQuestion
    { _uuid = question4 ^. uuid
    , _title = "EDITED: " ++ question4 ^. title
    , _text = Just $ "EDITED: " ++ (fromJust $ question4 ^. text)
    , _requiredLevel = Just 2
    , _tagUuids = question4 ^. tagUuids
    , _referenceUuids = question4 ^. referenceUuids
    , _expertUuids = question4 ^. expertUuids
    , _itemTemplateQuestionUuids = [q4_it1_question6 ^. uuid, q4_it1_question5 ^. uuid]
    }

question4WithNewType :: Question
question4WithNewType =
  IntegrationQuestion
    { _uuid = question4 ^. uuid
    , _title = question4 ^. title
    , _text = question4 ^. text
    , _requiredLevel = question4 ^. requiredLevel'
    , _tagUuids = question4 ^. tagUuids
    , _referenceUuids = question4 ^. referenceUuids
    , _expertUuids = question4 ^. expertUuids
    , _integrationUuid = ontologyPortal ^. uuid
    , _integrationProps = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question4Plain :: Question
question4Plain =
  ListQuestion
    { _uuid = question4 ^. uuid
    , _title = question4 ^. title
    , _text = question4 ^. text
    , _requiredLevel = question4 ^. requiredLevel'
    , _tagUuids = question4 ^. tagUuids
    , _referenceUuids = question4 ^. referenceUuids
    , _expertUuids = question4 ^. expertUuids
    , _itemTemplateQuestionUuids = []
    }

q4_it1_question5 :: Question
q4_it1_question5 =
  ListQuestion
    { _uuid = fromJust $ U.fromString "9f8b1681-f6dc-4fdb-8e38-018df91fd2bd"
    , _title = "Fifth Question"
    , _text = Just "Some funny description"
    , _requiredLevel = Just 1
    , _tagUuids = [tagBioInformatic ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _itemTemplateQuestionUuids = [q4_it1_q5_it2_question7 ^. uuid, q4_it1_q5_it2_question8 ^. uuid]
    }

q4_it1_question5Plain :: Question
q4_it1_question5Plain =
  ListQuestion
    { _uuid = q4_it1_question5 ^. uuid
    , _title = q4_it1_question5 ^. title
    , _text = q4_it1_question5 ^. text
    , _requiredLevel = q4_it1_question5 ^. requiredLevel'
    , _tagUuids = q4_it1_question5 ^. tagUuids
    , _referenceUuids = q4_it1_question5 ^. referenceUuids
    , _expertUuids = q4_it1_question5 ^. expertUuids
    , _itemTemplateQuestionUuids = []
    }

q4_it1_question5Edited :: Question
q4_it1_question5Edited =
  ListQuestion
    { _uuid = q4_it1_question5 ^. uuid
    , _title = "EDITED: Fifth Question"
    , _text = Just "EDITED: Some funny description"
    , _requiredLevel = Just 3
    , _tagUuids = q4_it1_question5 ^. tagUuids
    , _referenceUuids = q4_it1_question5 ^. referenceUuids
    , _expertUuids = q4_it1_question5 ^. expertUuids
    , _itemTemplateQuestionUuids = [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
    }

q4_it1_question6 :: Question
q4_it1_question6 =
  OptionsQuestion
    { _uuid = fromJust $ U.fromString "efcf425f-f5c6-4c36-9aaf-fd4ced17adf5"
    , _title = "Sixth Question"
    , _text = Just "Some non-funny description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = [km1_ch2_q6_r1 ^. uuid, km1_ch2_q6_r2 ^. uuid]
    , _expertUuids = [km1_ch2_q6_eAlbert ^. uuid, km1_ch2_q6_eNikola ^. uuid]
    , _answerUuids = [q4_it1_q6_answerNo ^. uuid, q4_it1_q6_answerYes ^. uuid]
    }

q4_it1_question6Edited :: Question
q4_it1_question6Edited =
  OptionsQuestion
    { _uuid = q4_it1_question6 ^. uuid
    , _title = "Sixth Question"
    , _text = Just "Some non-funny description"
    , _requiredLevel = Nothing
    , _tagUuids = []
    , _referenceUuids = [km1_ch2_q6_r2 ^. uuid, km1_ch2_q6_r1 ^. uuid]
    , _expertUuids = [km1_ch2_q6_eNikola ^. uuid, km1_ch2_q6_eAlbert ^. uuid]
    , _answerUuids = [q4_it1_q6_answerYes ^. uuid, q4_it1_q6_answerNo ^. uuid]
    }

q4_it1_q5_it2_question7 :: Question
q4_it1_q5_it2_question7 =
  ValueQuestion
    { _uuid = fromJust $ U.fromString "385026a5-c35b-4461-9588-bcbc10c99ac5"
    , _title = "Seventh Question"
    , _text = Just "Some non-funny description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _valueType = StringQuestionValueType
    }

q4_it1_q5_it2_question8 :: Question
q4_it1_q5_it2_question8 =
  ValueQuestion
    { _uuid = fromJust $ U.fromString "f272a0b6-14fd-477f-8a95-d7ea483a4395"
    , _title = "Eighth Question"
    , _text = Just "Some non-funny description"
    , _requiredLevel = Just 2
    , _tagUuids = []
    , _referenceUuids = []
    , _expertUuids = []
    , _valueType = StringQuestionValueType
    }

question9 :: Question
question9 =
  IntegrationQuestion
    { _uuid = fromJust $ U.fromString "ebadd964-4605-4550-998c-30b1f4e51239"
    , _title = "Ninth Question"
    , _text = Just "Some nice description"
    , _requiredLevel = Just 1
    , _tagUuids = [tagBioInformatic ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _integrationUuid = ontologyPortal ^. uuid
    , _integrationProps = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question9Edited :: Question
question9Edited =
  IntegrationQuestion
    { _uuid = question9 ^. uuid
    , _title = "EDITED: " ++ (question9 ^. title)
    , _text = Just $ "EDITED: " ++ (fromJust $ question9 ^. text)
    , _requiredLevel = Just 4
    , _tagUuids = [tagDataScience ^. uuid]
    , _referenceUuids = question9 ^. referenceUuids
    , _expertUuids = question9 ^. expertUuids
    , _integrationUuid = question9 ^. integrationUuid'
    , _integrationProps = Map.fromList [("domain", "biology"), ("country", "de")]
    }

question9PropsEdited :: Question
question9PropsEdited =
  IntegrationQuestion
    { _uuid = question9 ^. uuid
    , _title = question9 ^. title
    , _text = question9 ^. text
    , _requiredLevel = question9 ^. requiredLevel'
    , _tagUuids = question9 ^. tagUuids
    , _referenceUuids = question9 ^. referenceUuids
    , _expertUuids = question9 ^. expertUuids
    , _integrationUuid = question9 ^. integrationUuid'
    , _integrationProps = Map.fromList [("domain", "biology"), ("language", "")]
    }

question9WithNewType :: Question
question9WithNewType =
  ValueQuestion
    { _uuid = question9 ^. uuid
    , _title = "EDITED: " ++ question9 ^. title
    , _text = question9 ^. text
    , _requiredLevel = question9 ^. requiredLevel'
    , _tagUuids = question9 ^. tagUuids
    , _referenceUuids = question9 ^. referenceUuids
    , _expertUuids = question9 ^. expertUuids
    , _valueType = DateQuestionValueType
    }

question9ConvertedToValue :: Question
question9ConvertedToValue =
  ValueQuestion
    { _uuid = question9 ^. uuid
    , _title = question9 ^. title
    , _text = question9 ^. text
    , _requiredLevel = question9 ^. requiredLevel'
    , _tagUuids = question9 ^. tagUuids
    , _referenceUuids = question9 ^. referenceUuids
    , _expertUuids = question9 ^. expertUuids
    , _valueType = StringQuestionValueType
    }

question10 :: Question
question10 =
  IntegrationQuestion
    { _uuid = fromJust $ U.fromString "5f65baf2-4103-4417-a47a-73d622ec4e44"
    , _title = "Tenth Question"
    , _text = Just "Some nice description"
    , _requiredLevel = Nothing
    , _tagUuids = [tagBioInformatic ^. uuid]
    , _referenceUuids = []
    , _expertUuids = []
    , _integrationUuid = bioPortal ^. uuid
    , _integrationProps = Map.fromList [("domain", "legal"), ("branch", "mammal")]
    }

question10ConvertedToValue :: Question
question10ConvertedToValue =
  ValueQuestion
    { _uuid = question10 ^. uuid
    , _title = question10 ^. title
    , _text = question10 ^. text
    , _requiredLevel = question10 ^. requiredLevel'
    , _tagUuids = question10 ^. tagUuids
    , _referenceUuids = question10 ^. referenceUuids
    , _expertUuids = question10 ^. expertUuids
    , _valueType = StringQuestionValueType
    }
