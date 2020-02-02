module Proton.Diffraction where

import Data.Distributive
import Proton.Types
import Data.Profunctor

diffract :: Distributive f => Optic (Star f) s t a b -> (a -> f b) -> s -> f t
diffract opt f = runStar $ opt (Star f)
