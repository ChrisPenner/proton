module Data.Profunctor.Extras where

import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Data.Profunctor.Rep
import Data.Function ((&))
import Control.Monad

join' :: (Sieve p f, Strong p) => p a (p a b) -> p a (f b)
join' = strong (&) . rmap sieve

join'' :: (Representable p, Strong p, Monad (Rep p)) => p a (p a b) -> p a b
join'' = tabulate . rmap join . sieve . strong (&) . rmap sieve

absorb :: (Representable p, m ~ Rep p, Monad m) => p a (m b) -> p a b
absorb = tabulate . fmap join . sieve

newtype Dub p f a b = Dub (p (f a) (f b))

instance (Profunctor p, Functor f) => Profunctor (Dub p f) where
  dimap f g (Dub p) = Dub (dimap (fmap f) (fmap g) p)
