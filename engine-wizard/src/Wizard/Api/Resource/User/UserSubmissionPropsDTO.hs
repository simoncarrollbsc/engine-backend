module Wizard.Api.Resource.User.UserSubmissionPropsDTO where

import qualified Data.Map.Strict as M
import GHC.Generics

data UserSubmissionPropsDTO =
  UserSubmissionPropsDTO
    { _sId :: String
    , _name :: String
    , _values :: M.Map String String
    }
  deriving (Generic, Eq, Show)

instance Ord UserSubmissionPropsDTO where
  compare a b = compare (_sId a) (_sId b)
