{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Getter where

import Data.Functor.Contravariant
import Data.Profunctor

type Getter s t a b = forall p. (forall x. Contravariant (p x), forall x. Functor (p x), Profunctor p) => p a b -> p s t

to :: (s -> a) -> Getter s t a b
to f = phantom . lmap f

view :: Getter s t a b -> s -> a
view g = runForget . g $ Forget id

views :: Getter s t a b -> (a -> a') -> s -> a'
views g f = f . view g

like :: a -> Getter s t a b
like = to . const
