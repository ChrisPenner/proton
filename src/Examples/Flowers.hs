{-# LANGUAGE TypeFamilies #-}
module Examples.Flowers where

import Proton

import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable

import Data.List
import Data.Ord
import Data.Function
import Control.Applicative

data Species = Setosa | Versicolor | Virginica
  deriving Show

-- data Measurements = Measurements [Float]
--   deriving Show

data Measurements = Measurements {getMeasurements :: [Float]}
  deriving Show


data Flower = Flower {species :: Species, measurements :: Measurements}
  deriving Show

measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) = sum . fmap abs $ zipWith (-) xs ys

classify :: ([Flower], Measurements) -> Flower
classify (flowers, m) =
    let Flower species _ = minimumBy (comparing (measurementDistance m . measurements)) flowers
     in Flower species m

-- aggregate :: Kaleidoscope' Measurements Float
-- aggregate p = cotabulate thingy
--   where
--       foldFunc = cosieve p
--       thingy measurements =
--           Measurements . getZipList . fmap foldFunc . sequenceA . fmap (ZipList . getMeasurements) $ measurements

aggregate :: Kaleidoscope' Measurements Float
aggregate = iso getMeasurements Measurements . pointWise

-- measureNearest :: ListLens' Flower Measurements
-- measureNearest p = cotabulate thingy
--   where
--       -- foldFunc :: (Foldable f) => f Measurements -> Measurements
--       foldFunc = cosieve p
--       -- thingy :: (Foldable f) => f Flower -> Flower
--       thingy flowers =
--           classify (toList flowers, foldFunc (fmap measurements flowers))

measureNearest :: ListLens' Flower Measurements
measureNearest = listLens measurements classify

flower1 :: Flower
flower1 = Flower Versicolor (Measurements [2, 3, 4, 2])

flower2 :: Flower
flower2 = Flower Setosa (Measurements [5, 4, 3, 2.5])

flowers :: [Flower]
flowers = [flower1, flower2]

mean :: [Float] -> Float
mean [] =  0
mean xs = sum xs / fromIntegral (length xs)

test :: IO ()
test = do
    -- We can use a list-lens as a setter over a single element
    print $ flower1 & measureNearest . aggregate %~ negate

    -- We can explicitly compare to a specific result
    print $ Measurements [5, 4, 3, 1] & flowers ?. measureNearest

    -- We can provide an aggregator explicitly
    print $ mean & (flowers >- measureNearest . aggregate)
    print $ flowers & (measureNearest . aggregate *% mean)
