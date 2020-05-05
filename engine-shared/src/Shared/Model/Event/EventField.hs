module Shared.Model.Event.EventField where

import Data.Typeable
import GHC.Generics

data EventField a
  = NothingChanged
  | ChangedValue a
  | DirectValue a
  deriving (Show, Eq, Generic, Typeable)

instance Functor EventField where
  fmap f (DirectValue a) = DirectValue (f a)
  fmap f (ChangedValue a) = ChangedValue (f a)
  fmap _ NothingChanged = NothingChanged
