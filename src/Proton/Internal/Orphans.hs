{-# OPTIONS_GHC -fno-warn-orphans #-}
module Proton.Internal.Orphans where

import Data.Profunctor
import Control.Comonad
import Proton.Types




-- Don't think this is useful for anything; but we can get a Strong instance for Costar
-- using an adjunction between Star/Costar through (,) (->).

-- instance Strong (Costar ((,) e)) where
--   second' co = costarFlip $ second' (starFlip co)

-- starFlip :: Costar ((,) e) a b -> Star ((->) e) a b
-- starFlip (Costar f) = Star (\e a ->  f (a, e))

-- costarFlip :: Star ((->) e) a b -> Costar ((,) e) a b
-- costarFlip (Star f) = Costar (\(e, a) ->  f a e)
