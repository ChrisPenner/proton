{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Flowers where

import Proton

import Data.Profunctor.Rep
import Data.Profunctor.Strong
import Data.Profunctor.Sieve
import Data.Foldable

import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.Function
import Control.Applicative
import Debug.Trace
import Data.Functor.Identity
import Proton.Algebraic
import Data.Profunctor

data Species = Setosa | Versicolor | Virginica
  deriving Show

data Measurements = Measurements {getMeasurements :: [Float]}
  deriving Show

data Flower = Flower {flowerSpecies :: Species, flowerMeasurements :: Measurements}
  deriving Show

-- measurements' :: p [Float] [Float] -> p Measurements Measurements
-- measurements' :: Lens' Measurements [Float]
-- measurements' p =

measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) =
    sqrt . sum $ zipWith diff xs ys
  where
    diff a b = (a - b) ** 2

-- aggregate :: Kaleidoscope' Measurements Float
aggregate :: Kaleidoscope' Measurements Float
aggregate = iso getMeasurements Measurements . pointWise

-- measureLens :: Lens' Measurements [Float]
-- measureLens = lens getMeasurements setter
--   where
--     setter _ b = Measurements b

classify :: [Flower] -> Measurements -> Maybe Flower
classify flowers m
  | null flowers = Nothing
  | otherwise =
  let Flower species _ = minimumBy
                          (comparing (measurementDistance m . flowerMeasurements))
                          flowers
   in Just $ Flower species m

-- measurements :: (Corepresentable p, Foldable (Corep p)) => Optic' p Flower Measurements
-- measurements = listLens flowerMeasurements classify

measurements :: AlgebraicLens Flower (Maybe Flower) Measurements Measurements
measurements = listLens flowerMeasurements classify

-- strained :: forall s b. ListLens s [s] s Bool
-- strained = listLens id go
--   where
--     -- go :: ([s], [Bool]) -> [s]
--     -- go  = fmap fst . filter snd . uncurry zip
--     go  (x, True)  = x
--     go  (x, False) = []

versicolor :: Flower
versicolor = Flower Versicolor (Measurements [2, 3, 4, 2])

setosa :: Flower
setosa = Flower Setosa (Measurements [5, 4, 3, 2.5])

flowers :: [Flower]
flowers = [versicolor, setosa]

mean :: Fractional a => [a] -> a
mean [] =  0
mean xs = sum xs / fromIntegral (length xs)


infixr 4 >--
(>--) :: [s] -> Optic (Costar []) s t a a -> t
(>--) xs opt = (runCostar $ opt (Costar head)) xs

aggregateWith :: Functor f => (f Float -> Float) -> Optic (Costar []) Measurements Measurements Float Float
aggregateWith aggregator p = Costar (Measurements . fmap (cosieve p) . transpose . fmap getMeasurements)

avgMeasurement :: Foldable f => f Measurements -> Measurements
avgMeasurement ms = Measurements (mean <$> groupedMeasurements)
  where
    groupedMeasurements :: [[Float]]
    groupedMeasurements = transpose (getMeasurements <$> toList ms)
    mean :: [Float] -> Float
    mean xs = sum xs / fromIntegral (length xs)

applyWeight :: Float -> Measurements -> Measurements
applyWeight w (Measurements m) = Measurements (fmap (*w) m)

partitioned :: forall f a. (Ord a) => AlgebraicLens a ([a], [a]) a a
partitioned = listLens id splitter
  where
    -- splitter :: f a -> a -> ([a], [a])
    splitter xs ref
      = (filter (< ref) (toList xs), filter (>= ref) (toList xs))

onFirst :: Eq a => AlgebraicLens (a, b) (Maybe b) a a
onFirst = listLens fst picker
  where
    picker xs a = lookup a $ toList xs

selectingOn :: (s -> a) -> AlgebraicLens s (Maybe s) a (Maybe Int)
selectingOn project = listLens project picker
  where
    picker xs i = (toList xs !!) <$> i

indexOf :: Eq s => AlgebraicLens s (Maybe Int) s s
indexOf = listLens id (flip elemIndex . toList)

test :: IO ()
test = do
    -- print $ [1..10] & partitioned ?- (5 :: Int)
    -- print $ [1..10] & partitioned >- mean
    -- print $ ["banana", "pomegranate", "watermelon"] & selectingOn length >- elemIndex 11
    -- print $ ["banana", "pomegranate", "watermelon"] & selectingOn length . indexOf ?- 11
    -- print $ Identity "banana" & selectingOn length . indexOf %~ (+10)
    -- print $ (flowers >-- (measurements . aggregateWith mean))
    print $ flowers & (measurements . aggregate >- mean)
    -- We can use a list-lens as a setter over a single element
    -- print $ versicolor & measurements . aggregate %~ negate

    -- -- We can explicitly compare to a specific result
    -- print $ (flowers !! 1) ^. measurements
    -- print $ (flowers ?. measurements) $ Measurements [5, 4, 3, 1]
    -- print $ Measurements [5, 4, 3, 1] & (measurements .* flowers)
    -- print $ Measurements [5, 4, 3, 1] & measurements .* flowers

    -- -- We can provide an aggregator explicitly
    -- print $ mean & (flowers >- measurements . aggregate)
    -- print $ flowers & measurements >- avgMeasurement
    -- print $ M.fromList [(1.2, setosa), (0.6, versicolor)] & measurements >- avgMeasurement . fmap (uncurry applyWeight) . M.toList
    -- print $ flowers & (measurements . aggregate *% mean)
    -- print $ flowers & (measurements . aggregate *% mean)
    -- print $ flowers & (measurements . aggregate *% maximum)
    -- print $ [[1, 2, 3], [1, 2, 3], [1, 2, 3]] & convolving *% id
    --


allMeasurements :: [[Float]]
allMeasurements =
      [ [1  , 2  , 3  , 4  ]
      , [10 , 20 , 30 , 40 ]
      , [100, 200, 300, 400]
      ]

measurementMap :: M.Map String (ZipList Float)
measurementMap = M.fromList
      [ ("setosa"    , ZipList [1  , 2  , 3  , 4  ])
      , ("versicolor", ZipList [10 , 20 , 30 , 40 ])
      , ("virginica" , ZipList [100, 200, 300, 400])
      ]
