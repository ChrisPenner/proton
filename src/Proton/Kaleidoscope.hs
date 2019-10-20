{-# LANGUAGE TypeFamilies #-}
module Proton.Kaleidoscope where

import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Proton.Lens
import Data.Profunctor.Rep

-- type Kaleidoscope f s t a b = forall p. Strong p =>  p (f a) b -> p (f s) t
-- type Kaleidoscope f s t a b = Lens (f s) t (f a) b
type Kaleidoscope f s t a b = forall p. (Corep p ~ f, Corepresentable p) => p a b -> p s t
type Kaleidoscope' f s a = Kaleidoscope f s s a a


-- type Collide s t a b = forall p. Profunctor p => p a b -> p s t
-- collider :: f ((f a -> b) -> f s -> t) -> Collide s t a b
-- collider = 

-- kaleidoscope :: forall s t a b f. (s -> a) -> (f s -> b -> t) -> Kaleidoscope f s t a b
-- -- kaleidoscope project select = strong select . lmap (fmap project)
-- kaleidoscope project select = cotabulate . strong select . go . cosieve
--   where
--     go :: (f a -> b) -> f s -> b
--     go f = f . fmap project


-- maxOf :: Ord a => Kaleidoscope' Pair s a -> s -> s -> s
-- maxOf k a b = k maximum $ Pair a b

-- maxing :: (Foldable f, Functor f) => Kaleidoscope f s t a a -> f s -> t
-- maxing k = runCostar $ k (Costar maximum)


maxing :: (Ord a, Foldable f, Functor f) => (Costar f a a -> Costar f s t) -> f s -> t
maxing k = runCostar $ k (Costar maximum)
