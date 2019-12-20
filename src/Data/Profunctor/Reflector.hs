{-# LANGUAGE ScopedTypeVariables #-}
module Data.Profunctor.Reflector where

import Data.Tagged

import Data.Profunctor
import Data.Profunctor.MStrong
import Data.Distributive

class MStrong p => Reflector p where
  reflected :: Applicative f => p a b -> p (f a) (f b)

instance Reflector (->) where
  reflected = fmap

instance Traversable f => Reflector (Costar f) where
  reflected (Costar f) = Costar (fmap f . sequenceA)

instance Reflector Tagged where
  reflected (Tagged b) = Tagged (pure b)

instance Distributive f => Reflector (Star f) where
  reflected (Star f) = Star (collect f)

-- instance (MStrong p, Corepresentable p, Traversable f, Corep p ~ f) => Reflector p where
--   reflected p = cotabulate . go $ cosieve p
--     where
--       go :: forall g a b. Applicative g => (f a -> b) -> f (g a) -> (g b)
--       go f fga = f <$> sequenceA fga
