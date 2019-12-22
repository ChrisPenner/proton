module Proton.Achromatic where

import Data.Profunctor
import Data.Profunctor.Strong
import Proton.Lens

achrom :: forall s t a b
       . (s -> Maybe (b -> t))
       -> (s -> a)
       -> (b -> t)
       -> Lens s t a b
achrom try proj rev p = strong go $ lmap proj p
  where
    go :: s -> b -> t
    go s b = case try s of
        Nothing -> rev b
        Just f -> f b
