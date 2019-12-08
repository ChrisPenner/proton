module Examples.Flowers where

import Proton

import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable

import Data.List
import Data.Ord
import Data.Function

data Species = Setosa | Versicolor | Virginica
  deriving Show

-- data Measurements = Measurements [Float]
--   deriving Show

type Measurements = [Float]


data Flower = Flower {_species :: Species, _measurements :: Measurements}
  deriving Show

measurementDistance :: Measurements -> Measurements -> Float
measurementDistance xs ys = sum . fmap abs $ zipWith (-) xs ys

-- distance :: LensLike f s t a b
-- distance

-- measureNearest :: ((Measurements, Measurements) -> f Measurements) -> (Flower, Flower) -> f Flower
-- ListLens((Flower, Flower), (Measurements, Measurements))

classify :: ([Flower], Measurements) -> Flower
classify (flowers, m) =
    let Flower species _ = minimumBy (comparing (measurementDistance m . _measurements)) flowers
     in Flower species m

measureNearest :: ListLens' Flower Measurements
measureNearest p = cotabulate thingy
  where
      -- foldFunc :: (Foldable f) => f Measurements -> Measurements
      foldFunc = cosieve p
      -- thingy :: (Foldable f) => f Flower -> Flower
      thingy flowers =
          classify (toList flowers, foldFunc (fmap _measurements flowers))

flower1 :: Flower
flower1 = Flower Versicolor [2, 3, 4, 2]

flower2 :: Flower
flower2 = Flower Setosa [5, 4, 3, 2.5]

flowers :: [Flower]
flowers = [flower1, flower2]

mean :: [Measurements] -> Measurements
mean = fmap avg . transpose
  where
    avg [] = 0
    avg xs = sum xs / fromIntegral (length xs)

test :: IO ()
test = do
    -- We can use a list-lens as a setter over a single element
    print $ flower1 & measureNearest %~ fmap negate

    -- We can explicitly compare to a specific result
    print $ [5, 4, 3, 1] & flowers ?. measureNearest

    -- We can provide an aggregator explicitly
    print $ mean & (flowers >- measureNearest)
    print $ flowers & (measureNearest *% mean)

