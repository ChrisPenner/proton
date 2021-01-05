module Data.Profunctor.Phantom where

import Data.Profunctor
import qualified Data.Functor.Contravariant as C
import Data.Profunctor.Cayley

class Profunctor p => Phantom p where
    phantom :: p a x -> p a y

instance Phantom (Forget r) where
  phantom (Forget f) = Forget f

instance (Functor f, C.Contravariant f) => Phantom (Star f) where
  phantom (Star f) = Star (C.phantom . f)

instance (Functor f, Phantom p) => Phantom (Cayley f p) where
  phantom (Cayley fpab) = Cayley (fmap phantom fpab)
