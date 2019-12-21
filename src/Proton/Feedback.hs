module Proton.Feedback where

import Data.Profunctor
import Proton.Types
import Proton.Setter
import Data.Function
import Debug.Trace

type Feedback s t a b = forall p. Costrong p => p a b -> p s t
type Feedback' s a = Feedback s s a a

-- Simplified:
-- feedback :: forall p s t a b state. Costrong p
--      => ((a, b) -> b) -> (b -> (a, b)) -> Optic' p a b
feedback :: forall p s t a b. Costrong p
     => ((s, b) -> a) -> (b -> (t, b)) -> Optic p s t a b
feedback ping pong = unfirst . dimap ping pong

-- factorial :: Feedback' Int Int
-- factorial = feedback ping pong
--   where
--     ping :: (Int, Int) -> Int
--     ping ~(new, acc) = new * acc
--     pong :: Int -> (Int, Int)
--     pong n = (n - 1, 3)

fib :: Feedback Int [Int] [Int] [Int]
fib = feedback ping pong
  where
    ping :: ((Int, [Int]) -> [Int])
    ping (n, xs) = take n xs
    pong :: [Int] -> ([Int], [Int])
    pong xs = (reverse xs, xs)

-- >>> 10 & fib %~ \xs -> 1 : 1 : zipWith (+) xs (tail xs)
-- [1,1,2,3,5,8,13,21,34,55,89]
