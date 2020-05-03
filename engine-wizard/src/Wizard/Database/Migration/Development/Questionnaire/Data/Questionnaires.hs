module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Questionnaire.QuestionnaireUtil
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import qualified Wizard.Service.User.UserMapper as U_Mapper

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { _uuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
    , _name = "My Private Questionnaire"
    , _level = 1
    , _accessibility = PrivateQuestionnaire
    , _packageId = germanyPackage ^. pId
    , _selectedTagUuids = []
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = fReplies
    , _labels = fLabels
    , _ownerUuid = Just $ userAlbert ^. uuid
    , _creatorUuid = Just $ userAlbert ^. uuid
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  Questionnaire
    { _uuid = questionnaire1 ^. uuid
    , _name = "EDITED" ++ (questionnaire1 ^. name)
    , _level = 3
    , _accessibility = PublicQuestionnaire
    , _packageId = questionnaire1 ^. packageId
    , _selectedTagUuids = questionnaire1 ^. selectedTagUuids
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = questionnaire1 ^. replies
    , _labels = fLabelsEdited
    , _ownerUuid = Nothing
    , _creatorUuid = Just $ userAlbert ^. uuid
    , _createdAt = questionnaire1 ^. createdAt
    , _updatedAt = questionnaire1 ^. updatedAt
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto = toSimpleDTO questionnaire1 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert)

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { _name = questionnaire1 ^. name
    , _packageId = questionnaire1 ^. packageId
    , _accessibility = questionnaire1 ^. accessibility
    , _tagUuids = []
    , _templateUuid = questionnaire1 ^. templateUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { _name = questionnaire1Edited ^. name
    , _accessibility = questionnaire1Edited ^. accessibility
    , _level = 1
    , _replies =
        toReplyDTO <$>
        [ rQ1
        , rQ2
        , rQ2_aYes_fuQ1
        , rQ3
        , rQ4
        , rQ4_it1_q5
        , rQ4_it1_q5_it1_question7
        , rQ4_it1_q5_it1_question8
        , rQ4_it1_q6
        , rQ4_it2_q5
        , rQ4_it2_q6
        , rQ9
        , rQ10
        ]
    , _labels = []
    , _templateUuid = Nothing
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { _uuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , _name = "My PublicReadOnly Questionnaire"
    , _level = questionnaire1 ^. level
    , _accessibility = PublicReadOnlyQuestionnaire
    , _packageId = germanyPackage ^. pId
    , _selectedTagUuids = []
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = fReplies
    , _labels = fLabels
    , _ownerUuid = Just $ userAlbert ^. uuid
    , _creatorUuid = Just $ userAlbert ^. uuid
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { _uuid = questionnaire2 ^. uuid
    , _name = "EDITED" ++ (questionnaire2 ^. name)
    , _level = 3
    , _accessibility = PublicQuestionnaire
    , _packageId = questionnaire2 ^. packageId
    , _selectedTagUuids = questionnaire2 ^. selectedTagUuids
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = questionnaire2 ^. replies
    , _labels = fLabelsEdited
    , _ownerUuid = Nothing
    , _creatorUuid = Just $ userAlbert ^. uuid
    , _createdAt = questionnaire2 ^. createdAt
    , _updatedAt = questionnaire2 ^. updatedAt
    }

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto = toSimpleDTO questionnaire2 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert)

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { _uuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , _name = "My Public Questionnaire"
    , _level = questionnaire1 ^. level
    , _accessibility = PublicQuestionnaire
    , _packageId = germanyPackage ^. pId
    , _selectedTagUuids = []
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = fReplies
    , _labels = fLabels
    , _ownerUuid = Nothing
    , _creatorUuid = Nothing
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire3Edited :: Questionnaire
questionnaire3Edited =
  Questionnaire
    { _uuid = questionnaire3 ^. uuid
    , _name = "EDITED" ++ (questionnaire3 ^. name)
    , _level = 3
    , _accessibility = PrivateQuestionnaire
    , _packageId = questionnaire3 ^. packageId
    , _selectedTagUuids = questionnaire3 ^. selectedTagUuids
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = questionnaire3 ^. replies
    , _labels = fLabelsEdited
    , _ownerUuid = Just $ userAlbert ^. uuid
    , _creatorUuid = Nothing
    , _createdAt = questionnaire3 ^. createdAt
    , _updatedAt = questionnaire3 ^. updatedAt
    }

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyPackage QSDefault Nothing

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { _uuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
    , _name = "Outdated Questionnaire"
    , _level = 2
    , _accessibility = PrivateQuestionnaire
    , _packageId = netherlandsPackage ^. pId
    , _selectedTagUuids = []
    , _templateUuid = Just $ commonWizardTemplate ^. uuid
    , _formatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _replies = []
    , _labels = []
    , _ownerUuid = Nothing
    , _creatorUuid = Nothing
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire4PublicReadOnly :: Questionnaire
questionnaire4PublicReadOnly = questionnaire4 {_accessibility = PublicReadOnlyQuestionnaire}

questionnaire4Public :: Questionnaire
questionnaire4Public = questionnaire4 {_accessibility = PublicQuestionnaire, _ownerUuid = Nothing}

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
    {_uuid = fromJust (U.fromString "5deabef8-f526-421c-90e2-dd7aed1a25c5"), _packageId = netherlandsPackageV2 ^. pId}

questionnaire4PublicReadOnlyUpgraded :: Questionnaire
questionnaire4PublicReadOnlyUpgraded = questionnaire4Upgraded {_accessibility = PublicReadOnlyQuestionnaire}

questionnaire4PublicUpgraded :: Questionnaire
questionnaire4PublicUpgraded = questionnaire4Upgraded {_accessibility = PublicQuestionnaire, _ownerUuid = Nothing}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fReplies :: [Reply]
fReplies =
  [ rQ1
  , rQ2
  , rQ2_aYes_fuQ1
  , rQ3
  , rQ4
  , rQ4_it1_q5
  , rQ4_it1_q5_it1_question7
  , rQ4_it1_q5_it1_question8
  , rQ4_it1_q6
  , rQ4_it2_q5
  , rQ4_it2_q6
  , rQ9
  , rQ10
  ]

rQ1 :: Reply
rQ1 =
  Reply
    { _path = createReplyKey [U.toString $ chapter1 ^. uuid, U.toString $ question1 ^. uuid]
    , _value = StringReply "Reply to 1st question"
    }

rQ2 :: Reply
rQ2 =
  Reply
    { _path = createReplyKey [U.toString $ chapter1 ^. uuid, U.toString $ question2 ^. uuid]
    , _value = AnswerReply $ q2_answerYes ^. uuid
    }

rQ2_aYes_fuQ1 :: Reply
rQ2_aYes_fuQ1 =
  Reply
    { _path =
        createReplyKey
          [ U.toString $ chapter1 ^. uuid
          , U.toString $ question2 ^. uuid
          , U.toString $ q2_answerYes ^. uuid
          , U.toString $ q2_aYes_fuQuestion1 ^. uuid
          ]
    , _value = AnswerReply $ q2_aYes_fuq1_answerNo ^. uuid
    }

rQ3 :: Reply
rQ3 =
  Reply
    { _path = createReplyKey [U.toString $ chapter2 ^. uuid, U.toString $ question3 ^. uuid]
    , _value = AnswerReply $ q3_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ4 :: Reply
rQ4 =
  Reply
    {_path = createReplyKey [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid], _value = ItemListReply 2}

rQ4_it1_q5 :: Reply
rQ4_it1_q5 =
  Reply
    { _path =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "0", U.toString $ q4_it1_question5 ^. uuid]
    , _value = ItemListReply 1
    }

rQ4_it1_q5_it1_question7 :: Reply
rQ4_it1_q5_it1_question7 =
  Reply
    { _path =
        createReplyKey
          [ U.toString $ chapter2 ^. uuid
          , U.toString $ question4 ^. uuid
          , "0"
          , U.toString $ q4_it1_question5 ^. uuid
          , "0"
          , U.toString $ q4_it1_q5_it2_question7 ^. uuid
          ]
    , _value = StringReply "Ai1: q5: Ai1: Reply to 7th question"
    }

rQ4_it1_q5_it1_question8 :: Reply
rQ4_it1_q5_it1_question8 =
  Reply
    { _path =
        createReplyKey
          [ U.toString $ chapter2 ^. uuid
          , U.toString $ question4 ^. uuid
          , "0"
          , U.toString $ q4_it1_question5 ^. uuid
          , "0"
          , U.toString $ q4_it1_q5_it2_question8 ^. uuid
          ]
    , _value = StringReply "Ai1: q5: Ai1: Reply to 8th question"
    }

rQ4_it1_q6 :: Reply
rQ4_it1_q6 =
  Reply
    { _path =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "0", U.toString $ q4_it1_question6 ^. uuid]
    , _value = AnswerReply $ q4_it1_q6_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ4_it2_q5 :: Reply
rQ4_it2_q5 =
  Reply
    { _path =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "1", U.toString $ q4_it1_question5 ^. uuid]
    , _value = ItemListReply 0
    }

rQ4_it2_q6 :: Reply
rQ4_it2_q6 =
  Reply
    { _path =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "1", U.toString $ q4_it1_question6 ^. uuid]
    , _value = AnswerReply $ q4_it1_q6_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ9 :: Reply
rQ9 =
  Reply
    { _path = createReplyKey [U.toString $ chapter3 ^. uuid, U.toString $ question9 ^. uuid]
    , _value = IntegrationReply {_integrationValue = PlainValue "Plain reply to 9st question"}
    }

rQ10 :: Reply
rQ10 =
  Reply
    { _path = createReplyKey [U.toString $ chapter3 ^. uuid, U.toString $ question10 ^. uuid]
    , _value =
        IntegrationReply
          { _integrationValue =
              IntegrationValue {_intId = "bsg-p000007", _intValue = "Integration reply to 9st question"}
          }
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fLabels :: [Label]
fLabels = [Label {_path = rQ1 ^. path, _value = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}]

fLabelsEdited :: [Label]
fLabelsEdited =
  [ Label {_path = rQ1 ^. path, _value = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}
  , Label {_path = rQ2 ^. path, _value = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}
  ]
