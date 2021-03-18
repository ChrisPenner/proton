module Data.Profunctor.StateT where

import Data.Profunctor
import Control.Category (Category)
import qualified Control.Category as C
import Data.Bifunctor (first)
import Data.Profunctor.State.Class

newtype StateT s p a b = StateT (p (a, s) (b, s))

instance Profunctor p => Profunctor (StateT s p) where
  dimap f g (StateT s) = StateT (dimap (first f) (first g) s)

instance (Category p) => Category (StateT s p) where
  id = StateT C.id
  StateT s . StateT t = StateT (s C.. t)

instance (Category p, Profunctor p) => ProfunctorState s (StateT s p) where
  state f = StateT (lmap f C.id)
