module Setter where

import Data.Profunctor
import Data.Profunctor.Rep

type Setter s t a b = (a -> b) -> (s -> t)
type Setter' s a = Setter s s a a

set :: Setter s t a b -> s -> b -> t
set l s b = l (const b) s

over :: Setter s t a b -> (a -> b) -> s -> t
over l f s = l f s

sets :: (forall p. Profunctor p => p a b -> p s t) -> Setter s t a b
sets = id

setter :: (s -> a) -> (b -> t) -> Setter s t a b
setter f g = dimap f g

mapped :: Functor f => Setter (f a) (f b) a b
mapped = fmap
