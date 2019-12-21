{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
module Proton.Glass where

import Data.Profunctor
import Data.Distributive
import Control.Comonad
import Proton.Types
import Proton.Lens
import Proton.Setter
import Proton.Getter
import Data.Functor.Rep

type Glass s t a b = forall p. (Strong p, Closed p) => Optic p s t a b
type Glass' s a = Glass s s a a

-- class Glassy p where
--   glass :: (((s -> a) -> b) -> s -> t) -> p a b -> p s t

type Glassed p = (Strong p, Closed p)
-- class (Strong p, Closed p) => Glassed p where
glassed :: (Strong p, Closed p) => p a b -> p (t, u -> a) (t, u -> b)
glassed = second' . closed

-- instance Glassed (->) where
-- instance (Functor f, Distributive f) => Glassed (Star f) where

glass :: forall p s t a b. Glassed p => (((s -> a) -> b) -> s -> t) -> p a b -> p s t
glass glasser p = dimap l r $ glassed p
  where
    l :: s -> (s, (s -> a) -> a)
    l s = (s, ($ s))
    r :: (s, (s -> a) -> b) -> t
    r (s, f) = glasser f s

lensGlass :: forall s t a b. Lens s t a b -> Glass s t a b
lensGlass lns = glass glasser
  where
    glasser :: ((s -> a) -> b) -> s -> t
    glasser gl s = set lns s (gl (view lns))

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
