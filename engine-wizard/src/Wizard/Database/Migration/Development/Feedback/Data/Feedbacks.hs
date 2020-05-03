module Wizard.Database.Migration.Development.Feedback.Data.Feedbacks where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Model.Feedback.Feedback

feedback1 :: Feedback
feedback1 =
  Feedback
    { _uuid = fromJust . U.fromString $ "c44c06d1-ad9f-4f73-9c05-2aa9eddacae1"
    , _issueId = 1
    , _questionUuid = question1 ^. uuid
    , _packageId = germanyPackage ^. pId
    , _title = "Provide more descriptive content"
    , _content = "I'm not very satisfied with a description of this question"
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

feedback1Create :: FeedbackCreateDTO
feedback1Create =
  FeedbackCreateDTO
    { _questionUuid = feedback1 ^. questionUuid
    , _packageId = feedback1 ^. packageId
    , _title = feedback1 ^. title
    , _content = feedback1 ^. content
    }

feedback2 :: Feedback
feedback2 =
  Feedback
    { _uuid = fromJust . U.fromString $ "22e24917-7443-40f7-a3f2-4ea9f69ceebb"
    , _issueId = 99999
    , _questionUuid = question1 ^. uuid
    , _packageId = germanyPackage ^. pId
    , _title = "Non-existing issue"
    , _content = "There is no issue like that"
    , _createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
