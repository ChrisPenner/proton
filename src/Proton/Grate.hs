module Proton.Grate where

import Data.Profunctor
import Data.Distributive
import Data.Function ((&), on)
import Data.Functor.Rep
import Proton.Types
import Data.Pair

type Grate s t a b = forall p. Closed p => (p a b) -> (p s t)
type Grate' s a = Grate s s a a

newtype Zipping a b = Zipping (a -> a -> b)

grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate g = dimap (&) g . closed

distributed :: (Closed p, Representable g) => p a b -> p (g a) (g b)
distributed = dimap index tabulate . closed

-- zipWithOf :: Grate s t a b -> (a -> a -> b) -> s -> s -> t
-- zipWithOf g =

-- zipWithOf :: forall p s t a b. Closed p => Optic p s t a b -> (a -> a -> b) -> s -> s -> t
-- zipWithOf g f s1 s2 = degrated $ \idx -> (f `on` idx) s1 s2
--     where
--       degrated :: ((s -> a) -> b) -> t
--       degrated = degrating g

zipWithOf :: forall s t a b. Optic (Costar Pair) s t a b -> (a -> a -> b) -> s -> s -> t
zipWithOf g f s1 s2 = zipFWithOf g (liftPair f) (Pair s1 s2)

-- degrating :: Grate s t a b -> p ((s -> a) -> b) t
-- degrating = undefined

-- Equivalent to `>-` from Algebraic lenses, but with different semantics
zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
zipFWithOf g fab fs = grated fs
  where
    grated :: f s -> t
    Costar grated = g (Costar fab)
