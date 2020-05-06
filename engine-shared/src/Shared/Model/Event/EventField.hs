module Shared.Model.Event.EventField where

import Data.Typeable
import GHC.Generics

data EventField a
  = NothingChanged
  | ChangedValue
      { _value :: a
      }
  | DirectValue
      { _value :: a
      }
  deriving (Show, Eq, Generic, Typeable)

instance Functor EventField where
  fmap f (DirectValue a) = DirectValue (f a)
  fmap f (ChangedValue a) = ChangedValue (f a)
  fmap _ NothingChanged = NothingChanged

instance Applicative EventField where
  pure = ChangedValue
  DirectValue f <*> m = fmap f m
  ChangedValue f <*> m = fmap f m
  NothingChanged <*> _m = NothingChanged

instance Monad EventField where
  (DirectValue x) >>= k = k x
  (ChangedValue x) >>= k = k x
  NothingChanged >>= _ = NothingChanged
  (>>) = (*>)
  fail _ = NothingChanged

instance Semigroup a => Semigroup (EventField a) where
    NothingChanged <> b       = b
    a       <> NothingChanged = a
    DirectValue a  <> DirectValue b  = DirectValue (a <> b)
    ChangedValue a  <> ChangedValue b  = ChangedValue (a <> b)

instance Semigroup a => Monoid (EventField a) where
    mempty = NothingChanged