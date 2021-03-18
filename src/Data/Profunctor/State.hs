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
  state (StateT p) = StateT (dimap (\(a, s) -> ((a, s), s)) fst p)

instance (Profunctor p) => ProfunctorState' s (StateT s p) where
  get' (StateT p) = StateT (lmap (\(a, s) -> ((a, s), s)) p)
  put' (StateT p) = StateT (rmap fst p)
