{-# LANGUAGE TypeFamilies #-}
module Proton.Grid where

import Data.Profunctor
import Data.Profunctor.Traversing
import Proton.Types
import Proton.Grate
import Data.Pair

type Grid s t a b = forall p. (Traversing p, Closed p) => Optic p s t a b
type Grid' s a = Grid s s a a

both :: Grid (a, a) (b, b) a b
both = paired . distributed

-- beside :: Optic p s t a b -> Optic p s' t' a b -> Optic p (s, s') (t, t') a b
-- beside l r = distributed
