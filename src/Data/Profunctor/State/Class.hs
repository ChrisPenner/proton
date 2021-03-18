{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.State.Class where

import Data.Profunctor
import Control.Category (Category, (>>>), (<<<))
import qualified Control.Category as C

class (C.Category p, Profunctor p) => ProfunctorState s p | p -> s where
  {-# MINIMAL state | get, put #-}
  -- get :: p (a, s) b -> p a b
  get :: p a (a, s)
  get = state (\(a, s) -> ((a, s), s))
  -- put :: p a (b, s) -> p a b
  put :: p (a, s) a
  put = state fst
  state :: ((a, s) -> (b, s)) -> p a b
  state f = (get >>> rmap f C.id >>> put)

--   get :: p (s, a) b -> p a b
--   get p = state (_ p)
--   put :: p a (s, b) -> p a b
--   put p = state (lmap snd p)
--   state :: p (s, a) (s, b) -> p a b
--   state f = put . get $ f
