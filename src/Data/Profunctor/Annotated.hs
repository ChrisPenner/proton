{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Profunctor.Annotated where

import Data.Profunctor
import Data.Tagged

class (Profunctor p, Profunctor q) => Annotatable e p q | q -> p where
    coindexed :: p a (e, b) -> q a b
    default coindexed :: (p ~ q) => p a (e, b) -> q a b
    coindexed = rmap snd

data Annotated e p a b = Annotated {runCoindexed :: (p a (e, b))}

instance Profunctor p => Profunctor (Annotated e p) where
  dimap f g (Annotated p) = Annotated (dimap f (second' g) p)

instance Strong p => Strong (Annotated i p) where
  second' (Annotated p) = Annotated (dimap id reassoc $ second' p)
    where
      reassoc (c, (i, b)) = (i, (c, b))

instance Profunctor p => Annotatable i p (Annotated i p) where
  coindexed p = Annotated p

instance Annotatable e (Forget r) (Forget r) where
  coindexed (Forget f) = (Forget f)

instance Annotatable e (->) (->) where
instance Functor f => Annotatable e (Star f) (Star f) where
instance Functor f => Annotatable e (Costar f) (Costar f) where
instance Annotatable e Tagged Tagged where
