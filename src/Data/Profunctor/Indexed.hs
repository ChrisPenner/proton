{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Profunctor.Indexed where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Tagged

class (Profunctor p, Profunctor q) => Indexable i p q | p -> q where
    indexed :: p a b -> q (i, a) b
    default indexed :: (p ~ q) => p a b -> q (i, a) b
    indexed = lmap snd

-- closeIndex :: (Closed q, Indexable i p q) => p a b -> q a (i -> b)
-- closeIndex = lmap (flip (,)) . closed  . indexed

data Indexed i p a b = Indexed (p (i, a) b)
newtype UnIndexed i p a b = UnIndexed (p a b)
  deriving newtype (Profunctor, Closed, Strong, Choice, Traversing, Cochoice, Costrong)

instance Profunctor p => Profunctor (Indexed i p) where
  dimap f g (Indexed p) = Indexed (dimap (second' f) g p)

instance Strong p => Strong (Indexed i p) where
  second' (Indexed p) = Indexed (dimap reassoc id $ second' p)
    where
      reassoc (i, (c, a)) = (c, (i, a))

instance Profunctor p => Indexable i (Indexed i p) p where
  indexed (Indexed p) = p

instance Profunctor p => Indexable i (UnIndexed i p) p where
  indexed (UnIndexed p) = lmap snd p

instance Indexable i (Forget r) (Forget r) where
instance Functor f => Indexable i (Star f) (Star f) where
instance Functor f => Indexable i (Costar f) (Costar f) where
instance Indexable i (->) (->) where
instance Indexable i Tagged Tagged where
