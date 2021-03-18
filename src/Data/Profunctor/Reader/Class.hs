{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Reader.Class where

import Data.Profunctor

class (Profunctor p) => ProfunctorReader r p | p -> r where
  {-# MINIMAL (ask | reader), local #-}
  ask :: p a (a, r)
  ask = reader (flip (,))
  reader :: (r -> a -> b) -> p a b
  reader f = rmap (uncurry (flip f)) ask
  local :: (r -> r) -> p a b -> p a b

class (Profunctor p) => ProfunctorReader' r p | p -> r where
  {-# MINIMAL ask', local' #-}
  ask' :: p (a, r) b -> p a b
  local' :: (r -> r) -> p a b -> p a b
