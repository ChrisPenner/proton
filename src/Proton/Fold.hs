{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE ImpredicativeTypes #-}
module Proton.Fold where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.Phantom
import Data.Monoid
import Proton.Types
import Data.Foldable

type Fold s t a b = forall p. (Traversing p, Phantom p) => p a b -> p s t

folding :: (Foldable f, Phantom p, Traversing p) => (s -> f a) -> p a b -> p s t
folding f = phantom . lmap (toList . f) . traverse'

folded :: (Traversing p, Foldable f, Phantom p)
       => p a b -> p (f a) t
folded = phantom . lmap toList . traverse'

foldOf :: Monoid a => Fold s t a b -> s -> a
foldOf f = runForget (f (Forget id))

foldMapOf :: Monoid m => Optic (Forget m) s t a b -> (a -> m) -> s -> m
foldMapOf f into = runForget (f (Forget into))

toListOf :: Optic (Forget [a]) s t a b -> s -> [a]
toListOf fld = foldMapOf fld pure

preview :: Optic (Forget (First a)) s t a b -> s -> Maybe a
preview fld = getFirst . foldMapOf fld (First . Just)

(^?) :: s -> Optic (Forget (First a)) s t a b -> Maybe a
(^?) = flip preview

(^..) :: s -> Optic (Forget [a]) s t a b -> [a]
(^..) = flip toListOf

(<+>) :: Semigroup r => Optic (Forget r) s t a b -> Optic (Forget r) s t' a b' -> Optic (Forget r) s t a b
(fldA <+> fldB) p = 
    case (fldA p, fldB (phantom p)) of
        (Forget f, Forget g) -> Forget (\a -> f a <> g a)
