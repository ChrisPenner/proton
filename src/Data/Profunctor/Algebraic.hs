{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Algebraic where

import Data.Profunctor
import Data.Tagged
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Data.Foldable
import Data.Functor.Identity
import Data.Proxy
import Control.Arrow
import Control.Applicative
import Data.Monoid
import Data.Distributive
import Data.Tuple
import Proton.Types

type AlgebraicLens s t a b = forall p. MStrong p => p a b -> p s t
type AlgebraicLens' s a = AlgebraicLens s s a a

class Profunctor p => MStrong p where
  mfirst' ::  Monoid m => p a b -> p (a, m) (b, m)
  mfirst' = dimap swap swap . msecond'
  msecond' ::  Monoid m => p a b -> p (m, a) (m, b)
  msecond' = dimap swap swap . mfirst'

  {-# MINIMAL mfirst' | msecond' #-}

instance MStrong (Forget r) where
  msecond' = second'

instance MStrong (->) where
  msecond' = second'

instance Functor f => MStrong (Star f) where
  msecond'  = second'

instance MStrong Tagged where
  msecond' (Tagged b) = Tagged (mempty, b)

instance Traversable f => MStrong (Costar f) where
  msecond' (Costar f) = Costar (go f)
    where
      go f fma = f <$> sequenceA fma

algebraic :: forall m p s t a b
           . (Monoid m,  MStrong p)
           => (s -> m)
           -> (s -> a)
           -> (m -> b -> t)
           -> Optic p s t a b
algebraic inject project flatten p
  = dimap (inject &&& id)  (uncurry flatten) $  strengthened
  where
    strengthened :: p (m, s) (m, b)
    strengthened = msecond' (lmap project p)

listLens :: MStrong p => (s -> a) -> ([s] -> b -> t) -> Optic p s t a b
listLens = algebraic pure

altLens :: (Alternative f, MStrong p) => (s -> a) -> (f s -> b -> t) -> Optic p s t a b
altLens project flatten = algebraic (Alt . pure)  project (flatten . getAlt)
