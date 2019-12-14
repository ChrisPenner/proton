module Proton.Glass where

import Data.Profunctor
import Control.Comonad
import Data.Distributive

class Glassy p where
  glass :: (((s -> a) -> b) -> s -> t) -> p a b -> p s t

instance Glassy (->) where
  glass glasser fab s = glasser go s
    where
      go sToA = fab $ sToA s

-- instance Comonad f => Glassy (Costar f) where
--   glass glasser (Costar fab) = Costar (\s -> extract $ fmap (glasser (go s)) s)
--     where
--       go fs sToA = fab $ fmap sToA fs

-- instance (Functor f) => Glassy (Costar f) where
--   glass glasser (Costar fab) = Costar (\fs -> flip glasser $ collect go fs)
--     where
--       go :: s -> _ -> b
--       go = undefined
--       go s sToA = fab $ fmap sToA fs


-- type Glass = Glassy p => p a b -> p s t

-- glass :: Comonad w => (((s -> a) -> b) -> s -> t) -> GrateLike w s t a b
-- glass glasser f s = (glasser $ \h -> f (h <$> s)) $ extract s
