module Proton.Loop where

import Data.Profunctor
import Proton.Types

-- Cochoice represents iteration/looping

type Loop s t a b = forall p. Cochoice p => p a b -> p s t
type Loop' s a = Loop s s a a

loop :: forall p s t a b. Cochoice p => (s -> a) -> (b -> Either a t) -> Optic p s t a b
loop inject step = unright . dimap (either id inject) step

