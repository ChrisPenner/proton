{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Review where

import Data.Profunctor
import Data.Tagged
import Data.Bifunctor
import Proton.Getter
import Data.Void

type Review t b = forall a s p. (Profunctor p, Bifunctor p) => p a b -> p s t

retagged :: (Profunctor p, Bifunctor p) => p a b -> p s b
retagged = first absurd . lmap absurd

review :: Review t b -> b -> t
review pab b = unTagged . pab $ Tagged b

infixr 8 #
(#) :: Review t b -> b -> t
(#) = review

reviews :: Review t b -> (t -> t') -> b -> t'
reviews r f = f . review r

re :: Review t b -> Getter b t
re r = to (review r)

unto :: (b -> t) -> Review t b
unto f = rmap f . retagged

un :: Getter s a -> Review a s
un g = unto (view g)
