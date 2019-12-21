module Proton.Grate where

import Data.Profunctor
import Data.Function ((&))
import Data.Functor.Rep
import Proton.Types
import Data.Pair
import Control.Comonad

type Grate s t a b = forall p. Closed p => (p a b) -> (p s t)
type Grate' s a = Grate s s a a

newtype Zipping a b = Zipping (a -> a -> b)

grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate g = dimap (&) g . closed

distributed :: (Closed p, Representable g) => p a b -> p (g a) (g b)
distributed = dimap index tabulate . closed

zipWithOf :: forall s t a b. Optic (Costar Pair) s t a b -> (a -> a -> b) -> s -> s -> t
zipWithOf g f s1 s2 = zipFWithOf g (liftPair f) (Pair s1 s2)

-- degrating :: Grate s t a b -> ((s -> a) -> b) -> t
-- degrating g f = undefined

-- Equivalent to `>-` from Algebraic lenses, but with different semantics
zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
zipFWithOf g fab fs = grated fs
  where
    grated :: f s -> t
    Costar grated = g (Costar fab)

-- extendThrough :: forall s t a b w. Comonad w => Grate s t a b -> (w a -> b) -> w s -> w t
-- extendThrough g f = extend (degrated . helper)
--   where
--     helper :: w s -> (s -> a) -> b
--     helper w' sToA = f (sToA <$> w')
--     degrated :: ((s -> a) -> b) -> t
--     degrated = degrating g

-- extendThrough :: forall s t a b w. Comonad w => Grate s t a b -> (w a -> b) -> w s -> w t
-- extendThrough g f = extend (degrated . helper)

-- (-<) :: Comonad w => Grate s t a b -> (w a -> b) -> w s -> w t
-- (-<) = extendThrough

