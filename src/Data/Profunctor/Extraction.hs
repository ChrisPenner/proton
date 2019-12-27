module Data.Profunctor.Extraction where

import Data.Profunctor
import Control.Comonad
import Data.Distributive
import qualified Data.List.NonEmpty as NE
import Control.Comonad.Store
import Data.Pair

class Profunctor p => Extraction p where
  extractions :: Comonad w => p (w a) b -> p (w a) (w b)

instance Extraction (Forget r) where
  extractions (Forget f) = Forget f

instance Extraction (->) where
  extractions f = extend f

instance Distributive f => Extraction (Star f) where
  extractions (Star f) = Star (distribute . extend f)

act :: (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t
act o f = runStar (o (Star f))

t :: (NE.NonEmpty a) -> Pair (Maybe a)
t (a NE.:| (b : _)) = Pair (Just a) (Just b)
t (a NE.:| _) = Pair (Just a) Nothing

u :: (NE.NonEmpty Int) -> Pair Int
u xs = Pair (sum xs) (NE.length xs)




-- thingy :: forall w a b. Comonad w => (a -> w (Either a b)) -> (w a -> w b)
-- thingy f = extend loop . fmap Left
--     where
--       loop :: w (Either a b) -> b
--       loop w = case extract w of
--           Left a' -> loop $ f a'
--           Right b -> b

home :: Int -> Store Int Int -> Either Int Int
home n s | extract s == n = Left $ pos s
  | abs (peeks (+1) s - n) < abs (peeks (subtract 1) s - n) = Right $ peeks (+1) s
  | otherwise = Right $ peeks (subtract 1) s

looper :: NE.NonEmpty Int -> Either [Int] Int
looper xs | sum xs > 10 = Left (NE.toList xs)
  | otherwise = Right $ (sum xs + 1)

coiterate :: forall w a b.
         (Traversable w, Comonad w)
         => (w a -> Either b a)
         -> (w a -> w b)
coiterate f = extend loop
    where
      loop :: w a -> b
      loop = either id loop . sequenceA . extend f

-- thingy :: (Choice p, Cochoice p) => p a (Either b a) -> p (w a) (w b)
-- thingy = _ . unright . _ . right'
