module Wizard.Service.Submission.SubmissionMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Model.Config.AppConfig

toSubmissionServiceSimpleDTO :: AppConfigSubmissionService -> SubmissionServiceSimpleDTO
toSubmissionServiceSimpleDTO config =
  SubmissionServiceSimpleDTO {_id = config ^. sId, _name = config ^. name, _description = config ^. description}
