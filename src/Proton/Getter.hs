{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Getter where

import Data.Functor.Contravariant
import Data.Profunctor
import Proton.Types

type Getter s t a b = forall p. (forall x. Contravariant (p x), forall x. Functor (p x), Profunctor p) => p a b -> p s t

to :: (s -> a) -> Getter s t a b
to f = phantom . lmap f

view :: Optic (Forget a) s t a b -> s -> a
view g = runForget . g $ Forget id

views :: Optic (Forget a) s t a b -> (a -> a') -> s -> a'
views g f = f . view g

like :: a -> Getter s t a b
like = to . const

infixl 8 ^.
(^.) ::  s -> Optic (Forget a) s t a b -> a
(^.) = flip view
