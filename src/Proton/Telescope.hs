module Proton.Telescope where

import Proton.Types
import Data.Profunctor.Expansive

-- Foldy optic for expanding structure.
cat :: Expansive p => Optic p [a] b a b
cat = expand
