module Proton.Algebraic where

import Data.Profunctor
import Data.Profunctor.MStrong
import Proton.Types
import Data.Monoid
import Control.Applicative
import Control.Arrow

type AlgebraicLens s t a b = forall p. MStrong p => p a b -> p s t
type AlgebraicLens' s a = AlgebraicLens s s a a

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
