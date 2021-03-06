{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Absorbent where

import Data.Profunctor
import Data.Functor.Identity
import Control.Monad

-- Similar to Profunctor Representable, but simpler to implement and less restrictive
-- Represents Profunctors which can run effects.
class Profunctor p => Absorbent m p | p -> m where
  absorb :: p a (m b) -> p a b

instance Absorbent Identity (->) where
  absorb p = runIdentity . p

instance Monad m => Absorbent m (Star m) where
  absorb (Star f) = Star (join . f)

instance Absorbent m (Forget (m r)) where
  absorb (Forget f) = Forget f
