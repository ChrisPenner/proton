{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Examples.Layers where

import Proton
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Algebraic
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe

imgLayers :: [[Int]]
imgLayers = [ [0, 1, 0, 1]
            , [2, 3, 3, 2]
            ]

done :: [Int]
done = imgLayers & pointWise *% head . dropWhile (== 0)

-- selector :: (Ord k, Corep p ~ M.Map k, Corepresentable p) => Optic p s [Maybe s] s [k]
-- selector = listLens id (\(m, k) -> flip M.lookup m <$> k)

-- done' = M.fromList [(1 :: Int, [1, 10]), (2, [2, 20]), (3, [3, 30])]  & selector . convolving *% M.findWithDefault 99 1

forward :: Profunctor p => (s -> a) -> Optic p s t a t
forward f = lmap f

back :: Profunctor p => (x -> t) -> Optic p s t s x
back f = rmap f

lookup'er :: Eq a => Algebraic p => Optic p (a, b) (a, Maybe b) a a
lookup'er = algebraic fst (\(xs, i) -> (i, lookup i xs))

test :: IO ()
test = do
    print $ [([1, 2] , "one" :: String), ([10, 0], "two")] & lookup'er . pointWise *% maximum
