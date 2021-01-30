{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Joinable where

import Data.Profunctor

class Profunctor p => Joinable p m | p -> m where
  join' :: p a (m b) -> p a b

instance Monad m => Joinable (Star m) m where
  join' (Star f) = Star (join . f)
    where
      join m = m >>= id

instance Monad m => Joinable (Forget (m r)) m where
  join' (Forget f) = Forget f
