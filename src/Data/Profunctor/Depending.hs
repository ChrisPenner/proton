module Data.Profunctor.Depending where

import Data.Profunctor
import Data.Profunctor.Traversing

class Traversing p => Depending p where
  depend :: (forall f. Monad f => (a -> f b) -> (s -> f t)) -> p a b -> p s t

instance Monad f => Depending (Star f) where
  depend f (Star amb) = Star (f amb)
