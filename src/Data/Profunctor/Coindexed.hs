{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Profunctor.Coindexed where

import Data.Profunctor
import Data.Void
import Control.Applicative
import Control.Monad
import Data.Tagged

class (Profunctor p, Profunctor q) => Coindexable e p q | q -> p where
    coindexed :: p a (Either e b) -> q a b
    default coindexed :: (e ~ Void, p ~ q) => p a (Either e b) -> q a b
    coindexed = rmap (either absurd id)

data Coindexed e p a b = Coindexed {runCoindexed :: (p a (Either e b))}

instance Profunctor p => Profunctor (Coindexed e p) where
  dimap f g (Coindexed p) = Coindexed (dimap f (right' g) p)

instance Choice p => Choice (Coindexed i p) where
  right' (Coindexed p) = Coindexed (dimap id reassoc $ right' p)
    where
      reassoc (Left c) = Right (Left c)
      reassoc (Right (Left i)) = Left i
      reassoc (Right (Right b)) = Right (Right b)

instance Profunctor p => Coindexable i p (Coindexed i p) where
  coindexed p = Coindexed p

instance Coindexable e (Forget r) (Forget r) where
  coindexed (Forget f) = (Forget f)

instance Coindexable Void (->) (->)

instance {-# OVERLAPPING #-} Functor f => Coindexable Void (Star f) (Star f) where
-- Could use selective applicative here instead.
instance (Alternative f, Monad f) => Coindexable e (Star f) (Star f) where
  coindexed (Star f) = Star $ f >=> either (const empty) pure
instance Functor f => Coindexable Void (Costar f) (Costar f) where
instance Coindexable Void Tagged Tagged where
