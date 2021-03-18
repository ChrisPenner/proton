module Examples.DSP where

import Data.Profunctor
import Data.Profunctor.Rep

blur :: Costar ((->) Int) Int Float
blur = cotabulate go
  where
    go :: (Int -> Int) -> Float
    go f = fromIntegral (f (-1) + f 0 + f 1) / 3


-- sample :: Monoid e => Costar ((->) e) a b -> e -> (e -> a) -> b
-- sample (Costar f) = f
