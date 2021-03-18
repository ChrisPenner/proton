{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.State.Class where

import Data.Profunctor
import Control.Category ((>>>))
import qualified Control.Category as C

class (C.Category p, Profunctor p) => ProfunctorState s p | p -> s where
  {-# MINIMAL state | (get, put) #-}
  get :: p a (a, s)
  get = state (rmap (\(a, s) -> ((a, s), s)) C.id)
  put :: p (a, s) a
  put = state (lmap (\((a, s), _) -> (a, s)) C.id)
  state :: p (a, s) (b, s) -> p a b
  state p = (get >>> p >>> put)


class (Profunctor p) => ProfunctorState' s p | p -> s where
  {-# MINIMAL (get', put') #-}
  get' :: p (a, s) b -> p a b
  put' :: p a (b, s) -> p a b
