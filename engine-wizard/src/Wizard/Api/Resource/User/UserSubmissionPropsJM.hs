module Wizard.Api.Resource.User.UserSubmissionPropsJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User

instance FromJSON UserSubmissionProps where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON UserSubmissionProps where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON UserSubmissionPropsDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON UserSubmissionPropsDTO where
  toJSON = genericToJSON simpleOptions'''
