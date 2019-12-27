module Proton.Lens where

import Data.Profunctor
import Data.Profunctor.Strong
import Control.Arrow ((&&&))

type Lens s t a b = forall p. Strong p => p a b -> p s t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- lens getter setter = strong setter . lmap getter
lens getter setter = dimap (id &&& getter) (uncurry setter) . second'

_1 :: Lens (a, x) (b, x) a b
_1 = lens fst (\(_, x) b -> (b, x))


_2 :: Lens (x, a) (x, b) a b
_2 = lens snd (\(x, _) b -> (x, b))
