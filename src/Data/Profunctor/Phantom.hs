module Data.Profunctor.Phantom where

import Data.Profunctor

class Profunctor p => Phantom p where
    phantom :: p a x -> p a y

instance Phantom (Forget r) where
  phantom (Forget f) = Forget f
