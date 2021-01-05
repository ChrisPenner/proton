{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Proton.Review where

import Data.Profunctor
import Data.Tagged
import Data.Bifunctor
import Proton.Getter
import Data.Void

type Review s t a b = forall p. (Profunctor p, Bifunctor p) => p a b -> p s t

retagged :: forall p a b s. (Profunctor p, Bifunctor p) => p a b -> p s b
retagged = first absurd . lmap absurd

review :: (Tagged a b -> Tagged s t) -> b -> t
review pab b = unTagged . pab $ Tagged b

infixr 8 #
(#) :: (Tagged a b -> Tagged s t) -> b -> t
(#) = review

reviews :: (Tagged a b -> Tagged s t) -> (t -> t') -> b -> t'
reviews r f = f . review r

re :: (Tagged a b -> Tagged s t) -> Getter b a t s
re r = to' (review r)

unto :: forall (s :: *) t (a :: *) b. (b -> t) -> (Tagged a b -> Tagged s t)
unto f = rmap f . retagged

un :: Getter s t a b -> (Tagged t s -> Tagged b a)
un g = unto (view g)
