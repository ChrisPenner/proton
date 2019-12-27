{-# LANGUAGE TypeFamilies #-}
module Proton.Grid where

import Data.Profunctor
import Data.Profunctor.Traversing
import Proton.Types
import Proton.Grate
import Data.Pair
import Proton.Setter
import Proton.Traversal

type Grid s t a b = forall p. (Traversing p, Closed p) => Optic p s t a b
type Grid' s a = Grid s s a a

-- bothT :: Traversal (a, a) (b, b) a b
-- bothT p = _ $ traverse' p

-- beside :: Optic p s t a b -> Optic p s' t' a b -> Optic p (s, s') (t, t') a b
-- beside l r = distributed
