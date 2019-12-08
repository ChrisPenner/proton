{-# LANGUAGE TypeFamilies #-}
module Proton.Kaleidoscope where

import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Proton.Types
import Data.Profunctor.Rep


import Control.Applicative
import Data.Ord
import Data.List
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep
import Data.Foldable


type ListLens s t a b = forall p. (Foldable (Corep p),  Corepresentable p) => p a b -> p s t
type ListLens' s a = ListLens s s a a


(?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?.) xs f a = (runCostar $ f (Costar (const a))) xs

(>-) :: f s -> Optic (Costar f) s t a b -> (f a -> b) -> t
(>-) xs opt aggregator = (runCostar $ opt (Costar aggregator)) xs

(*%) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(*%) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs

(.*) :: Optic (Costar f) s t a b -> f s -> b -> t
(.*) opt xs b = (runCostar $ opt (Costar (const b))) xs

