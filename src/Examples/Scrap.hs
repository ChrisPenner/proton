module Examples.Scrap where

import Linear
import Proton
import Data.Ord
import Data.List
import Data.Profunctor.Distributing
import Data.Profunctor.Algebraic
import Data.Profunctor.Closed
import Data.Profunctor.Rep
import Debug.Trace
import qualified Data.Map as M
import Data.Distributive
import Control.Applicative
import Data.Functor.Identity

data Species = Setosa | Versicolor | Virginica
  deriving Show

data Measurements = Measurements {getMeasurements :: V4 Float}
  deriving Show


data Flower = Flower {species :: Species, measurements :: Measurements}
  deriving Show

measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) = sum . abs $  xs - ys

classify :: Foldable f => (f Flower, Measurements) -> Flower
classify (flowers, m) =
    let Flower species _ = minimumBy (comparing (measurementDistance m . measurements)) flowers
     in Flower species m

aggregate :: (Corepresentable p, Traversable (Corep p), Closed p) => Optic' p Measurements Float
aggregate = iso getMeasurements Measurements . convolving

measureNearest :: forall f p. Foldable f => Algebraic f p => Optic' p Flower Measurements
measureNearest = algebraic measurements (classify @f)

flower1 :: Flower
flower1 = Flower Versicolor (Measurements (V4 2 3 4 2))

flower2 :: Flower
flower2 = Flower Setosa (Measurements (V4 5 4 3 2.5))

flowers :: [Flower]
flowers = [flower1, flower2, flower1]

mean :: [Float] -> Float
mean [] =  0
mean xs = sum xs / fromIntegral (length xs)

compVectors :: (Applicative f, Algebraic f p) => Optic p Int (f Int) Int (f Int)
compVectors = algebraic id (uncurry $ liftA2 (+))

test :: IO ()
test = do
    -- We can use a list-lens as a setter over a single element
    -- print $ flower1 & measureNearest . aggregate %~ negate

    -- -- We can explicitly compare to a specific result
    -- print $ Measurements [5, 4, 3, 1] & flowers ?. measureNearest
    -- print $ Measurements [5, 4, 3, 1] & measureNearest .* flowers

    -- -- We can provide an aggregator explicitly
    -- print $ mean & (flowers >- measureNearest . aggregate)
    -- print $ flowers & (measureNearest . aggregate *% mean)
    -- print $ flowers & measureNearest . aggregate *% maximum . traceShowId
    print . sequenceA $ [V2 1 2, V2 10 20, V2 100 200] & distribute' . compVectors *% const [2, 3, 4]
    -- print $ flower1 & measureNearest . aggregate %~ (+10)
    -- print $ flower1 ^. measureNearest . aggregate
    -- print $ [[1, 2, 3], [3, 4, 5]] & convolving *% mean
