{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Writer.Class where

import Data.Profunctor
import qualified Control.Category as C
import Data.Bifunctor (second)

class (Monoid w, C.Category p, Profunctor p) => ProfunctorWriter w p | p -> w where
  {-# MINIMAL (writer | tell), listen, pass #-}
  writer :: w -> p a a
  writer w = lmap (\a -> (a, w)) tell
  tell :: p (a, w) a
  tell = pass (rmap (second (\x -> (<> x))) C.id)
  listen :: p a b -> p a (b, w)
  pass :: p a (b, w -> w) -> p a b
