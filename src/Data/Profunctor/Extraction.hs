module Data.Profunctor.Extraction where

import Data.Profunctor
import Control.Comonad
import Data.Distributive

class Profunctor p => Extraction p where
  extractions :: Comonad w => p (w a) b -> p (w a) (w b)

instance Extraction (Forget r) where
  extractions (Forget f) = Forget f

instance Extraction (->) where
  extractions f = extend f

instance Distributive f => Extraction (Star f) where
  extractions (Star f) = Star (distribute . extend f)
