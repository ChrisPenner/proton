{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Profunctor.Applying where

import Data.Tagged

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable
import Data.Distributive
import Control.Applicative

-- traverse' :: Traversable f => p a b -> p (f a) (f b)
-- wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

class Profunctor p => Applying p where
  applying :: Applicative f => p a b -> p (f a) (f b)
  applying p = applyingBy pure (<*>) p
  applyingBy :: (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> p a b -> p (f a) (f b)

instance Applying (->) where
  applying = fmap
  applyingBy pure' apper f = apper (pure' f)

instance Traversable f => Applying (Costar f) where
  applying (Costar f) = Costar (fmap f . sequenceA)
  applyingBy pure' apper (Costar f) = Costar (apper (pure' f) . sequenceBy pure' apper)

-- Use free applicative here
sequenceBy :: Traversable t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> t (f a) -> f (t a)
sequenceBy _ _ _ = undefined

instance Applying Tagged where
  applying (Tagged b) = Tagged (pure b)
  applyingBy pure' _ (Tagged b) = Tagged (pure' b)

-- instance (Corepresentable p, Traversable (Corep p)) => Applying p  where
--   applying p = cotabulate (fmap (cosieve p) . sequenceA)

