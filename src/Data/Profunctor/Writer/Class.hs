{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Writer.Class where

import Data.Profunctor
import qualified Control.Category as C
import Data.Bifunctor (second)

class (Monoid w, C.Category p, Profunctor p) => ProfunctorWriter w p | p -> w where
  {-# MINIMAL listen, pass #-}
  tell :: p (a, w) a
  tell = lmap (second (\w -> (<> w))) pass
  listen :: p a (a, w)
  pass :: p (a, w -> w) a

class (Monoid w, Profunctor p) => ProfunctorWriter' w p | p -> w where
  {-# MINIMAL listen', pass' #-}
  tell' :: p a (b, w) -> p a b
  tell' p = pass' (rmap (\(b, w) -> (b, (<> w))) p)
  listen' :: p a b -> p a (b, w)
  pass' :: p a (b, w -> w) -> p a b
