{-# LANGUAGE InstanceSigs #-}
module Proton.Loop where

import Data.Profunctor
import Data.Profunctor.Traversing
import Proton.Types
import Data.Profunctor.Choice

-- Cochoice represents iteration/looping

type Loop s t a b = forall p. Cochoice p => p a b -> p s t
type Loop' s a = Loop s s a a

data CoPrism a b s t = CoPrism (s -> a) (b -> Either a t)

instance Profunctor (CoPrism a b) where
  dimap f g (CoPrism project match) = CoPrism (project . f) (fmap g . match)

instance Cochoice (CoPrism a b) where
  unright :: forall d a' b'. CoPrism a b (Either d a') (Either d b') -> CoPrism a b a' b'
  unright (CoPrism project match) = CoPrism (project . Right) (go . match)
    where
      go :: Either a (Either d b') -> Either a b'
      go (Left a) = Left a
      go (Right (Right b)) = Right b
      go (Right (Left d)) = Left (project $ Left d)

loop :: forall p s t a b. Cochoice p
     => (s -> a) -> (b -> Either a t) -> Optic p s t a b
loop inject step = unright . dimap (either id inject) step

-- loop' :: forall p s t a b. (Cochoice p, Traversing p)
--      => (s -> a) -> (b -> Either a t) -> Optic p s t a b
-- loop' inject step = unright . dimap (either id inject) step


-- iterM :: forall s t a b . Optic (Star ((,) [a])) s t a b -> (a -> Either a b) -> s -> ([a], t)
-- iterM o f s = g s
--   where
--     Star (g :: s -> ([a], t)) = (o . unright . lmap (either id id)) $ Star (wrapper . f)
--     wrapper :: Either a b -> ([a], Either a b)
--     wrapper (Left a) = ([a], Left a)
--     wrapper (Right b) = ([], Right b)

iterM :: forall s t a . Optic (Star ((,) [a])) s t a a -> (a -> Either a a) -> s -> ([a], t)
iterM o f s = g s
  where
    Star (g :: s -> ([a], t)) = o . unright . lmap (either id id) $ Star (wrapper . f)
    wrapper :: Either a a -> ([a], Either a a)
    wrapper (Left a) = ([a], Left a)
    wrapper (Right b) = ([b], Right b)




tester :: Int -> Either Int Int
tester a 
  | a < 10 = Left (succ a)
  | otherwise = Right (succ a)


-- iter :: forall f p s t a b. (Alternative f,  Cochoice p)
--      => Optic p s t a b
-- iter = _
