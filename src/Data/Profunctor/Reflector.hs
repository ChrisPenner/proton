{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
module Data.Profunctor.Reflector where

import Data.Tagged

import Data.Profunctor
import Control.Applicative
import Data.Profunctor.MStrong

class MStrong p => Reflector p where
  reflected :: Applicative f => p a b -> p (f a) (f b)

instance Reflector (->) where
  reflected = fmap

instance Traversable f => Reflector (Costar f) where
  reflected (Costar f) = Costar (fmap f . sequenceA)

instance Reflector Tagged where
  reflected (Tagged b) = Tagged (pure b)

