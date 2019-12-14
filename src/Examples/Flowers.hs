{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Flowers where

import Proton

import Data.Profunctor.Rep
import Data.Profunctor.Strong
import Data.Profunctor.Sieve
import Data.Foldable

import Data.List
import Data.Ord
import Data.Function
import Control.Applicative
import Debug.Trace
import Data.Functor.Identity
import Data.Profunctor.Applying
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

classify :: ([Flower], Measurements) -> Flower
classify (flowers, m) =
    let Flower species _ = minimumBy (comparing (measurementDistance m . flowerMeasurements)) flowers
     in Flower species m

measurements :: Algebraic p => Optic' p Flower Measurements
measurements = algebraic flowerMeasurements classify

-- strained :: forall s b. ListLens s [s] s Bool
-- strained = listLens id go
--   where
--     -- go :: ([s], [Bool]) -> [s]
--     -- go  = fmap fst . filter snd . uncurry zip
--     go  (x, True)  = x
--     go  (x, False) = []


flower1 :: Flower
flower1 = Flower Versicolor (Measurements [2, 3, 4, 2])

flower2 :: Flower
flower2 = Flower Setosa (Measurements [5, 4, 3, 2.5])

flowers :: [Flower]
flowers = [flower1, flower2]

mean :: [Float] -> Float
mean [] =  0
mean xs = sum xs / fromIntegral (length xs)

infixr 4 >--
(>--) :: [s] -> Optic (Costar []) s t a a -> t
(>--) xs opt = (runCostar $ opt (Costar head)) xs

aggregateWith :: Functor f => (f Float -> Float) -> Optic (Costar []) Measurements Measurements Float Float
aggregateWith aggregator p = Costar (Measurements . fmap (cosieve p) . transpose . fmap getMeasurements)

test :: IO ()
test = do
    -- print $ (flowers >-- (measurements . aggregateWith mean))
    -- We can use a list-lens as a setter over a single element
    print $ flower1 & measurements . aggregate %~ negate

    -- -- We can explicitly compare to a specific result
    -- print $ (flowers !! 1) ^. measurements
    -- print $ (flowers ?. measurements) $ Measurements [5, 4, 3, 1]
    -- print $ Measurements [5, 4, 3, 1] & (measurements .* flowers)
    -- print $ Measurements [5, 4, 3, 1] & measurements .* flowers

    -- -- We can provide an aggregator explicitly
    print $ mean & (flowers >- measurements . aggregate)
    -- print $ flowers & (measurements . aggregate *% mean)
    -- print $ flowers & (measurements . aggregate *% mean)
    -- print $ flowers & (measurements . aggregate *% maximum)
    -- print $ [[1, 2, 3], [1, 2, 3], [1, 2, 3]] & convolving *% id
    --
