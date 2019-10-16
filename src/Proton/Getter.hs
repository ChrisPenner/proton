{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Getter where

import Data.Functor.Contravariant
import Data.Profunctor

type Getter s a = forall p b t. (forall x. Contravariant (p x), forall x. Functor (p x), Profunctor p) => p a b -> p s t

to :: (s -> a) -> Getter s a
to f = phantom . lmap f

view :: Getter s a -> s -> a
view g = runForget . g $ Forget id

views :: Getter s a -> (a -> a') -> s -> a'
views g f = f . view g

like :: a -> Getter s a
like = to . const
