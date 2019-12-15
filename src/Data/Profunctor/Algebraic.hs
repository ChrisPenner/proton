{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Algebraic where

import Data.Profunctor
import Data.Tagged
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable
import Data.Functor.Identity
import Data.Proxy

class Profunctor p => Algebraic f p | p -> f where
  algebraic :: (s -> a) -> ((f s, b) -> t) -> p a b -> p s t

instance Functor f => Algebraic f (Costar f) where
  algebraic project flatten p = cotabulate run
    where
      run fs = flatten (fs, cosieve (lmap project p) fs)

instance Algebraic Identity (->) where
  algebraic project flatten p = run
    where
      run s = flatten (pure s, p . project $ s)

instance Algebraic Proxy Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten (Proxy, b))

instance Algebraic Proxy (Forget r) where
  algebraic project _flatten (Forget f) = Forget (f . project)
