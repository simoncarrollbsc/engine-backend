module Wizard.Model.ActionKey.ActionKey where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  deriving (Show, Eq, Generic)

data ActionKey =
  ActionKey
    { _uuid :: U.UUID
    , _userId :: U.UUID
    , _aType :: ActionKeyType
    , _hash :: String
    , _createdAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ActionKey where
  a == b = _uuid a == _uuid b && _userId a == _userId b && _aType a == _aType b && _hash a == _hash b
