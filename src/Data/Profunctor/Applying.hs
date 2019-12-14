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

class Profunctor p => Applying p where
  ap' :: Applicative f => p a b -> p (f a) (f b)

instance Applying (->) where
  ap' = fmap

instance Traversable f => Applying (Costar f) where
  ap' (Costar f) = Costar (fmap f . sequenceA)

instance Applying Tagged where
  ap' (Tagged b) = Tagged (pure b)

-- instance (Corepresentable p, Traversable (Corep p)) => Applying p  where
--   ap' p = cotabulate (fmap (cosieve p) . sequenceA)

