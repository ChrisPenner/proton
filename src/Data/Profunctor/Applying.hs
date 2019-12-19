{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
module Data.Profunctor.Applying where

import Data.Tagged

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Foldable
import Data.Distributive
import Control.Applicative
import Data.Functor.Identity
import Data.Proxy
import Data.Profunctor.Algebraic

data VoidF a

absurdF :: VoidF a -> x
absurdF q = case q of {}


-- traverse' :: Traversable f => p a b -> p (f a) (f b)
-- wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

class MStrong p => Kal g p | p -> g where
  convolute :: ((g a -> b) -> (g s -> t)) -> p a b -> p s t

convoluted :: forall p a b g f. (Applicative f, Traversable g) => Kal g p => p a b -> p (f a) (f b)
convoluted p = convolute go p
  where
    go f = (fmap f . sequenceA)

instance Kal Identity (->) where
  convolute lifter p = lifter (p . runIdentity) . Identity

-- Impossible?
-- instance Kal Proxy (Forget r) where
--   convolute lifter (Forget rToA) = Forget (_ $ lifter absurdF)


-- instance Functor f => Kal f (Costar f) where
  -- convolute embed p = _ p
  -- convoluted :: Applicative g => p a b -> p (g a) (g b)

class MStrong p => Applying p where
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

-- convoluted
--     :: (Traversable (Corep p), Applicative f, Corepresentable p)
--     => p a b
--     -> p (f a) (f b)
-- convoluted p = cotabulate (fmap (cosieve p) . sequenceA)

-- instance (Corepresentable p, Traversable (Corep p)) => Applying p  where
--   applying p = cotabulate (fmap (cosieve p) . sequenceA)

