module Examples.Diffract where

import Data.Pair
import Proton

split :: [a] -> Pair [a]
split x = Pair x (reverse x)

splitter :: (a, a) -> Pair a
splitter (a, b) = Pair a b

example :: IO ()
example = do
    print $ diffract (traversed . traversed) splitter [[(1, 2), (3, 4)], [(5, 6), (7, 8 :: Int)]]
