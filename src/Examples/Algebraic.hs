module Examples.Algebraic where

import Proton

pad :: AlgebraicLens String [String] String Int
pad = listLens id padder
  where
    padder :: [String] -> Int -> [String]
    padder xs n = fmap (\s -> take n $ s <> repeat ' ') xs

padLength :: AlgebraicLens String [String] Int Int
padLength = listLens length padder
  where
    padder :: [String] -> Int -> [String]
    padder xs n = fmap (\s -> take n $ s <> repeat ' ') xs

-- >>> ["a", "hello", "yo"] & padLength >- maximum
-- ["a    ","hello","yo   "]
-- >>> ["a", "hello", "yo"] & padLength >- minimum
-- ["a","h","y"]

-- >>> ["a", "hello", "yo"] & pad ?. 6
-- ["a     ","hello ","yo    "]
-- >>> ["a", "hello", "yo"] & pad ?. 2
-- ["a ","he","yo"]
-- >>> ["a", "hello", "yo"] & pad >- maximum . fmap length
-- ["a    ","hello","yo   "]
-- >>> ["a", "hello", "yo"] & pad >- minimum . fmap length
-- ["a","h","y"]
