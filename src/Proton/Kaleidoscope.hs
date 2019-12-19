{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Proton.Kaleidoscope where

-- ala http://events.cs.bham.ac.uk/syco/strings3-syco5/slides/roman.pdf
-- https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf

import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Proton.Types
import Proton.Lens
import Proton.Iso
import Data.Profunctor.Rep


import Control.Applicative
import Data.Ord
import Data.List
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep
import Data.Foldable
import Data.Profunctor.Applying
import Data.Profunctor.Algebraic


type Kaleidoscope s t a b = forall p. (Traversable (Corep p),  Corepresentable p) => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a

-- infixr 4 ?.
-- (?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
-- (?.) xs f a = (runCostar $ f (Costar (const a))) xs

infixr 4 ?-
(?-) :: Optic (Costar f) s t a b -> b -> f s -> t
(?-) opt b xs = (runCostar $ opt (Costar (const b))) xs

-- infixr 4 >-
-- (>-) :: f s -> Optic (Costar f) s t a b -> (f a -> b) -> t
-- (>-) xs opt aggregator = (runCostar $ opt (Costar aggregator)) xs


infixr 4 >-
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(>-) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs

-- ListLens s t a b -> (f a -> b) -> f s -> t
infixr 4 *%
(*%) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(*%) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs

-- ListLens s t a b -> f s -> b -> t
infixr 4 .*
(.*) :: Optic (Costar f) s t a b -> f s -> b -> t
(.*) opt xs b = (runCostar $ opt (Costar (const b))) xs

pointWise :: Kaleidoscope [a] [b] a b
pointWise = iso ZipList getZipList . convolving

cartesian :: Kaleidoscope [a] [b] a b
cartesian = convolving

convolving :: forall f a b. Applicative f => Kaleidoscope (f a) (f b) a b
convolving p = cotabulate (fmap (cosieve p) . sequenceA)

-- convolvingOf :: forall f a b. Applicative f => (Lens' (f a) a) -> Kaleidoscope' (f a) a
-- convolvingOf l p = cotabulate (\tf -> _ tf . cosieve p . fmap (view l) $ tf)

-- pointWiseOf :: ([a] -> a) -> Kaleidoscope' [a] a
-- pointWiseOf pick = dimap ZipList getZipList . convolvingOf (pick . getZipList)

-- collapse :: forall p f a b c g. (Traversable g, Applicative g, Alternative f, Corepresentable p, Corep p ~ g)
--         => Optic' p (f a) a
-- collapse p = cotabulate done
--   where
--     func :: g (f a) -> (f a)
--     func = cosieve (convolving p)
--     done :: g (f a) -> f a
--     done = func . pure @g . asum
