{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Examples.Loop where

import Proton.Loop
import Proton
import Data.Profunctor
import Data.Monoid

thing :: Loop' Int Int
thing = loop id step
  where
    step 0 = Right 0
    step n = Left n

-- factorial :: Int -> Maybe (Int, Product Int)
-- factorial 0 = Nothing
-- factorial n = Just (n - 1, Product n)

collatz :: Int -> [Int]
collatz n = n & while (/= 1) (:[]) %~ step
  where
    step x
      | even x = x `div` 2
      | otherwise = (3 * x) + 1

-- >>> collatz 3
-- [3,10,5,16,8,4,2]
-- >>> collatz 5
-- [5,16,8,4,2]

factorial :: Int -> Product Int
factorial n = n & while (/= 0) Product %~ subtract 1

-- >>> factorial 3
-- Product {getProduct = 6}
-- >>> factorial 6
-- Product {getProduct = 720}

accum :: forall p next state. (Monoid state, Strong p, Cochoice p)
      => (next -> Maybe (next, state)) -> Optic p next state next next
accum check = loop initialize step . _1
  where
    initialize n = case check n of
        Nothing -> (n, mempty)
        Just (_, s) -> (n, s)
    step :: (next, state) -> Either (next, state) state
    step (n, s) =
        case check n of
            Nothing -> Right s
            Just (n', s') -> Left (n', s <> s')

while ::
  (Monoid state, Strong p, Cochoice p) =>
  (t -> Bool) -> (t -> state) -> Optic p t state t t
while continue inj = accum $ \x -> if continue x then Just (x, inj x)
                                                    else Nothing

