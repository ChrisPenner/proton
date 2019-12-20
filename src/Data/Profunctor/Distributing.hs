module Data.Profunctor.Distributing where

import Data.Profunctor
import Data.Functor.Rep

distribute' :: (Closed p, Representable g) => p a b -> p (g a) (g b)
distribute' = dimap index tabulate . closed
