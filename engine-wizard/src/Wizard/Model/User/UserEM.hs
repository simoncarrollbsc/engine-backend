module Wizard.Model.User.UserEM where

import qualified Data.Map.Strict as M
import Shared.Util.Crypto (encryptAES256)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.User.User

instance SensitiveData User where
  process key entity = entity {_submissionProps = fmap (process key) (_submissionProps entity)}

instance SensitiveData UserSubmissionProps where
  process key entity = entity {_values = M.map (encryptAES256 key) (_values entity)}
