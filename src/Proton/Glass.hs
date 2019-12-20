{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Proton.Glass where

import Data.Profunctor
import Data.Distributive
import Control.Comonad

-- class Glassy p where
--   glass :: (((s -> a) -> b) -> s -> t) -> p a b -> p s t

class (Strong p, Closed p) => Glassed p where
  glassed :: p a b -> p (t, u -> a) (t, u -> b)
  glassed = second' . closed

glass' :: forall p s t a b. Glassed p => (((s -> a) -> b) -> s -> t) -> p a b -> p s t
glass' glasser p = dimap l r $ glassed p
  where
    l :: s -> (s, (s -> a) -> a)
    l s = (s, ($ s))
    r :: (s, (s -> a) -> b) -> t
    r (s, f) = glasser f s

-- Star
-- (->)

instance Glassed (->) where
instance (Functor f, Distributive f) => Glassed (Star f) where
-- instance Comonad f => Glassed (Costar f) where

-- instance Comonad f => Strong (Costar f) where
--   first' (Costar f) = (Costar (extract . extend (\x -> (, snd . extract $ x) $ f (fst <$> x))))



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
