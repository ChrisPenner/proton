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
