module Proton.Grate where

import Data.Profunctor
import Data.Distributive
import Data.Function ((&))

type Grate s t a b = forall p. Closed p => (p a b) -> (p s t)
type Grate' s a = Grate s s a a

newtype Zipping a b = Zipping (a -> a -> b)

grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate g = dimap (&) g . closed

-- cotraversed :: forall f a b. Distributive f => Grate (f a) (f b) a b
-- zipWithOf :: Grate s t a b -> (a -> a -> b) -> s -> s -> t
-- zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
-- collectOf :: forall f s t a b. Optic (Star f) s t a b -> (a -> f b) -> s -> f t
