module Wizard.Service.Feedback.FeedbackMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import LensesConfig
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Feedback.Feedback

toDTO :: Feedback -> String -> FeedbackDTO
toDTO feedback issueUrl =
  FeedbackDTO
    { _uuid = feedback ^. uuid
    , _issueId = feedback ^. issueId
    , _issueUrl = issueUrl
    , _questionUuid = feedback ^. questionUuid
    , _packageId = feedback ^. packageId
    , _title = feedback ^. title
    , _content = feedback ^. content
    , _createdAt = feedback ^. createdAt
    , _updatedAt = feedback ^. updatedAt
    }

fromCreateDTO :: FeedbackCreateDTO -> U.UUID -> Int -> UTCTime -> Feedback
fromCreateDTO dto fUuid issueId now =
  Feedback
    { _uuid = fUuid
    , _issueId = issueId
    , _questionUuid = dto ^. questionUuid
    , _packageId = dto ^. packageId
    , _title = dto ^. title
    , _content = dto ^. content
    , _createdAt = now
    , _updatedAt = now
    }

fromSimpleIssue :: Feedback -> IssueIDTO -> UTCTime -> Feedback
fromSimpleIssue feedback simpleIssue now =
  Feedback
    { _uuid = feedback ^. uuid
    , _issueId = simpleIssue ^. id
    , _questionUuid = feedback ^. questionUuid
    , _packageId = feedback ^. packageId
    , _title = simpleIssue ^. title
    , _content = simpleIssue ^. body
    , _createdAt = feedback ^. createdAt
    , _updatedAt = now
    }
