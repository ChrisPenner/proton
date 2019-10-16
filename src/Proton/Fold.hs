{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
module Proton.Fold where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Foldable
import Data.Functor.Contravariant

import Proton.Getter


type Fold r s t a b = Forget r a b -> Forget r s t

folding :: (Foldable f, Traversing p, forall x. Functor (p x), forall x. Contravariant (p x)) => (s -> f a) -> p a b -> p s t
folding f = to f . folded

folded :: (Foldable f, Traversing p, forall x. Contravariant (p x), forall x. Functor (p x))
       => p a b -> p (f a) (f b)
folded = phantom . lmap toList . traverse'

foldOf :: Fold a s t a b -> s -> a
foldOf f = runForget (f (Forget id))

foldMapOf :: Fold m s t a b -> (a -> m) -> s -> m
foldMapOf f into = runForget (f (Forget into))

toListOf :: Fold [a] s t a b -> s -> [a]
toListOf fld = foldOf (fld . to pure)
