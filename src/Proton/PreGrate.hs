{-# LANGUAGE TupleSections #-}
module Proton.PreGrate where

import Proton.Grate
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Profunctor
import Data.Maybe
import Proton.Types
import Data.Pair
import Data.Functor.Rep
import Data.Distributive
import Data.Functor.Contravariant
import Control.Comonad
import qualified Data.List.NonEmpty as NE
import Data.Profunctor.MStrong
import Data.Semigroup
import Data.Coerce


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

newtype Intersection a = Intersection (S.Set a)
instance Ord a => Semigroup (Intersection a) where
  (<>) = coerce S.intersection

fullAlignMap :: (Ord k, MStrong p, Closed p) => p a b -> p (M.Map k a) (M.Map k b)
fullAlignMap = dimap unpack rebuild . mfirst' . closed
  where
    unpack m = (\k -> fromJust $ M.lookup k m, Just . Intersection . M.keysSet $ m)
    rebuild (f, Just (Intersection ks)) = M.fromSet f ks
    rebuild (_, Nothing) = mempty

fullAlignList :: (MStrong p, Closed p) => p a b -> p [a] [b]
fullAlignList = dimap unpack rebuild . mfirst' . closed
  where
    unpack m = (\k -> m !! k, Just (Min (length m)))
    rebuild (_, Nothing) = []
    rebuild (f, Just (Min len)) = fmap f [0..len-1]

-- instance (Comonad f) => Strong (Costar f) where
--   first' (Costar f) = Costar ((\a b -> (a (fmap fst b), snd . extract $ b)) f)

-- doAThing :: Distributive f => Optic (Star f) s t a b -> (a -> Pair b) -> s -> Pair t
-- doAThing o = o (Star )

tester :: M.Map Int [String]
tester = zipFWithOf (fullAlignMap . fullAlignList) concat [x, y]


-- Allow splitting a record into component parts to zip as well as the "monoidal" remainder
-- Then zip the main components, mappend up the rest, and re-combine.
zipBy :: forall f p s t a b m. (Representable f, MStrong p, Closed p, Monoid m) => (s -> (f a, m)) -> (f b -> m -> t) -> p a b -> p s t
zipBy project embed = dimap unpack rebuild . mfirst' . closed
  where
    unpack :: s -> (Rep f -> a, m)
    unpack s = 
        case project s of
            (fa, m) -> (index fa, m)
    rebuild :: (Rep f -> b, m) -> t
    rebuild (rep, m) = embed (tabulate rep) m

-- Strong & Closed == don't need to specify indexes, maybe I can carry them through??

-- hoistO :: Optic (Star f) s t a b -> Optic (Costar g) s t a b

-- tester :: Star ((->) Bool) a b -> Costar Pair a b
-- tester (Star f) = Costar (_ f)

-- preZipWithOf :: forall s t a b. Optic (Star ((->) Bool)) s t a b -> (a -> a -> b) -> s -> s -> t
-- preZipWithOf g f s1 s2 = zipFWithOf g (_) (_)
--   where
--     buildPair p = tabulate p
--     applyPair (Pair a b) = f a b
--     -- thin :: a -> Bool -> b
--     -- thing = 

-- -- preZipFWithOf :: forall f s t a b. Optic (Star f) s t a b -> (a -> f b) -> (s -> f t)
