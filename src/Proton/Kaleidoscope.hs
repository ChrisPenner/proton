{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

type Kaleidoscope s t a b = forall p. (Traversable (Corep p),  Corepresentable p) => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a


infixr 4 ?.
(?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?.) xs f a = (runCostar $ f (Costar (const a))) xs

infixr 4 >-
(>-) :: f s -> Optic (Costar f) s t a b -> (f a -> b) -> t
(>-) xs opt aggregator = (runCostar $ opt (Costar aggregator)) xs

infixr 4 *%
(*%) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(*%) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs

infixr 4 .*
(.*) :: Optic (Costar f) s t a b -> f s -> b -> t
(.*) opt xs b = (runCostar $ opt (Costar (const b))) xs

pointWise :: Kaleidoscope [a] [b] a b
pointWise = dimap ZipList getZipList . convolving

cartesian :: Kaleidoscope [a] [b] a b
cartesian = convolving

convolving :: forall f a b. Applicative f => Kaleidoscope (f a) (f b) a b
convolving p = cotabulate (fmap (cosieve p) . sequenceA)

listLens :: (s -> a) -> (([s], b) -> t) -> ListLens s t a b
listLens project flatten p = cotabulate run
  where
      run s = flatten (toList s, cosieve p (project <$> s))
