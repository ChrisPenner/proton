module Data.Profunctor.Distributing where

import Data.Profunctor
import Data.Distributive
import Data.Functor.Rep
-- import Data.Profunctor.Rep
import Data.Profunctor.Sieve

distribute' :: (Closed p, Representable g) => p a b -> p (g a) (g b)
distribute' = dimap index tabulate . closed
