{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Getter where

import Data.Profunctor
import Data.Profunctor.Phantom
import Proton.Types

type Getter s t a b = forall p. Phantom p => p a b -> p s t

-- Getter without Phantom requirement, may be useful with Grids/Grates
to :: Profunctor p => (s -> a) -> Optic p s b a b
to f = lmap f

-- Getter based on Phantom
to' :: (s -> a) -> Getter s t a b
to' f = phantom . lmap f

-- Getter based on Forget explicitly
to'' :: (s -> a) -> Optic (Forget r) s t a b
to'' f (Forget g) = Forget (g . f)

view :: Optic (Forget a) s t a b -> s -> a
view g = runForget . g $ Forget id

views :: Optic (Forget a) s t a b -> (a -> a') -> s -> a'
views g f = f . view g

like :: a -> Getter s t a b
like = to' . const

infixl 8 ^.
(^.) ::  s -> Optic (Forget a) s t a b -> a
(^.) = flip view
