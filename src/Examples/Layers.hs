{-# LANGUAGE TypeFamilies #-}
module Examples.Layers where

import Proton
import Data.Profunctor
import Data.Profunctor.Rep
import qualified Data.Map as M
import Data.Foldable

imgLayers :: [[Int]]
imgLayers = [ [0, 1, 0, 1]
            , [2, 3, 3, 2]
            ]

done :: [Int]
done = imgLayers & pointWise *% head . dropWhile (== 0)

selector :: (Ord k, Corep p ~ M.Map k, Corepresentable p) => Optic p s [Maybe s] s [k]
selector = listLens id (\(m, k) -> flip M.lookup m <$> k)

done' = M.fromList [(1 :: Int, [1, 10]), (2, [2, 20]), (3, [3, 30])]  & selector . convolving *% M.findWithDefault 99 1

forward :: Profunctor p => (s -> a) -> Optic p s t a t
forward f = lmap f

back :: Profunctor p => (x -> t) -> Optic p s t s x
back f = rmap f
