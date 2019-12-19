{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Proton.Kaleidoscope (Reflector(..), Kaleidoscope, Kaleidoscope') where

-- ala http://events.cs.bham.ac.uk/syco/strings3-syco5/slides/roman.pdf
-- https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf

import Data.Profunctor
import Proton.Types
import Data.Profunctor.Rep
import Control.Applicative
import Data.Profunctor.Reflector

type Kaleidoscope s t a b = forall p. Reflector p => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a

pointWise :: Kaleidoscope [a] [b] a b
pointWise = dimap ZipList getZipList . reflected

-- collapse :: forall p f a b c g. (Traversable g, Applicative g, Alternative f, Corepresentable p, Corep p ~ g)
--         => Optic' p (f a) a
-- collapse p = cotabulate done
--   where
--     func :: g (f a) -> (f a)
--     func = cosieve (convolving p)
--     done :: g (f a) -> f a
--     done = func . pure @g . asum
