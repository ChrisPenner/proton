{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Proton.Internal.Orphans where

-- import Data.Profunctor
-- import Control.Comonad
-- import Proton.Types
-- import Proton.Setter
-- import Proton.Getter

-- Steal this from Cokleisli at some point
-- instance Comonad w => Choice (Costar w) where

-- instance Comonad f => Strong (Costar f) where
--   -- Not quite right
--   first' (Costar f) = (Costar (extract . extend (\x -> (, snd . extract $ x) $ f (fst <$> x))))

-- Don't think this is useful for anything; but we can get a Strong instance for Costar
-- using an adjunction between Star/Costar through (,) (->).

-- instance Strong (Costar ((,) e)) where
--   second' co = costarFlip $ second' (starFlip co)

-- starFlip :: Costar ((,) e) a b -> Star ((->) e) a b
-- starFlip (Costar f) = Star (\e a ->  f (a, e))

-- costarFlip :: Star ((->) e) a b -> Costar ((,) e) a b
-- costarFlip (Star f) = Costar (\(e, a) ->  f a e)
