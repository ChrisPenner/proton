module Data.Profunctor.Algebraic where

import Data.Profunctor
import Data.Tagged
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable

class Profunctor p => Algebraic p where
  algebraic :: (s -> a) -> (([s], b) -> t) -> p a b -> p s t

instance (Functor f, Foldable f) => Algebraic (Costar f) where
  algebraic project flatten p = cotabulate run
    where
      run s = flatten (toList s, cosieve (lmap project p) s)

instance Algebraic (->) where
  algebraic project flatten p = run
    where
      run s = flatten (pure s, p . project $ s)

instance Algebraic Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten ([], b))

instance Algebraic (Forget r) where
  algebraic project _flatten (Forget f) = Forget (f . project)
