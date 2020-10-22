{-# LANGUAGE TupleSections #-}
module Proton.PreGrate where

import Proton.Grate
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Profunctor
import Data.Maybe


alignMaybeWithDefault :: a -> Grate (Maybe a) (Maybe b) a b
alignMaybeWithDefault def = dimap (fromMaybe def) Just

aligner :: (s -> k -> a) -> ((k -> b) -> t) -> Grate s t a b
aligner index generate = dimap index generate . closed

alignMap :: (Ord k) => S.Set k -> Grate (M.Map k a) (M.Map k b) (Maybe a) (Maybe b)
alignMap keys = dimap (flip M.lookup) project . closed
  where
    project f = M.fromList . catMaybes $ ((\k -> (k,) <$> f k) <$> S.toList keys)

alignMapWithDefault :: (Ord k) => S.Set k -> a -> Grate (M.Map k a) (M.Map k b) a b
alignMapWithDefault keys def = alignMap keys . dimap (fromMaybe def) Just

alignList :: Int -> Grate [a] [b] (Maybe a) (Maybe b)
alignList bound = dimap (flip safeIndex) (\f -> catMaybes $ fmap f [0..bound-1] ) . closed
  where
    safeIndex :: Int -> [x] -> Maybe x
    safeIndex _ [] = Nothing
    safeIndex n _ | n < 0 = Nothing
    safeIndex 0 (x:_) = Just x
    safeIndex n (_:xs) = safeIndex (n-1) xs

alignListWithDefault :: Int -> a -> Grate [a] [b] a b
alignListWithDefault bound def = alignList bound . dimap (fromMaybe def) Just

alignMapMonoid :: (Monoid a, Ord k) => (S.Set k) -> Grate (M.Map k a) (M.Map k b) a b
alignMapMonoid n = alignMapWithDefault n mempty

alignListMonoid :: Monoid a => Int -> Grate [a] [b] a b
alignListMonoid n = alignListWithDefault n mempty

x :: M.Map Int [String]
x = M.fromList [(1, ["Gala", "Spartan", "Fuji"])]

y :: M.Map Int [String]
y = M.fromList [(1, ["Naval", "Mandarin"]), (2, ["Watermelon"])]

l :: Grate (M.Map Int [String]) (M.Map Int [b]) String b
l = alignMapWithDefault (S.fromList [1, 2, 3]) [] . alignListWithDefault 3 "def"

l' :: Grate (M.Map Int [String]) (M.Map Int [b]) (Maybe String) (Maybe b)
l' = alignMap (S.fromList [1, 2, 3]) . alignMaybeWithDefault [] . alignList 3


-- Strong & Choice == don't need to specify indexes, maybe I can carry them through??
