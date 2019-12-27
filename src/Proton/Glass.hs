{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
module Proton.Glass where

import Data.Profunctor
import Control.Comonad
import Proton.Types
import Proton.Internal.Orphans
import Proton.Lens
import Proton.Setter
import Proton.Getter

type Glass s t a b = forall p. (Strong p, Closed p) => Optic p s t a b
type Glass' s a = Glass s s a a

-- class Glassy p where
--   glass :: (((s -> a) -> b) -> s -> t) -> p a b -> p s t

type Glassed p = (Strong p, Closed p)
glassed :: (Strong p, Closed p) => p a b -> p (s, u -> a) (s, u -> b)
glassed = second' . closed

glass :: forall p s t a b. Glassed p => (((s -> a) -> b) -> s -> t) -> p a b -> p s t
glass glasser p = dimap l r $ glassed p
  where
    l :: s -> (s, (s -> a) -> a)
    l s = (s, ($ s))
    r :: (s, (s -> a) -> b) -> t
    r (s, f) = glasser f s

glassList :: forall a b. Glass [a] [b] a b
glassList = glass go
  where
    go :: (([a] -> a) -> b) -> [a] -> [b]
    go f s = undefined

extendOf :: (Comonad w) => Optic (Costar w) s t a b -> (w a -> b) -> w s -> w t
extendOf gr f = extend (runCostar $ gr (Costar f))

traversed' :: forall f a b. Traversable f => Glass (f a) (f b) a b
traversed' = glass go
  where
    go :: ((f a -> a) -> b) -> f a -> f b
    go f fa = undefined

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

-- Move this somewhere
-- instance (Comonad w) => Strong (Costar w) where
--   first' :: forall a b c. Costar w a b -> Costar w (a, c) (b, c)
--   first' (Costar f) = Costar go
--     where
--       go :: (w (a, c) -> (b, c))
--       go wac = extract $ extendThrough (lensGlass _1) f wac
