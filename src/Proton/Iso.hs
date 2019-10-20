{-# LANGUAGE DeriveFunctor #-}
module Proton.Iso where

import Data.Profunctor
import Proton.Getter
import Proton.Review
import Proton.Setter

type Iso s t a b = forall p. Profunctor p => p a b -> p s t
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

from :: Iso s t a b -> Iso b a t s
from i = withIso i $ flip iso

data Exchange a b s t =
    Exchange (s -> a) (b -> t)
  deriving Functor

instance Profunctor (Exchange a b) where
  dimap f' g' (Exchange f g) = Exchange (f . f') (g' . g)


withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso i handler =
    case i (Exchange id id) of
        Exchange f g -> handler f g

under :: Iso s t a b -> (t -> s) -> b -> a
under i ts b = withIso i $ \sa bt -> (sa . ts . bt $ b)

mapping :: (Functor f, Functor g) => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping i = dimap (fmap (view i)) (fmap (review i))

involuted :: (a -> a) -> Iso' a a
involuted f = iso f f
