module Proton.Prisms where

import Data.Profunctor
import Data.Tagged
import Data.Maybe

type Prism s t a b = forall p. Choice p => p a b -> p s t
type Prism' s a = forall p. Choice p => p a a -> p s s

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build split = rmap (either id build) . lmap split . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' build maybeGet = prism build (\s -> maybe (Left s) Right $ maybeGet s)

-- outside :: Representable p => APrism s t a b -> Lens (p t r) (p s r) (p b r) (p a r)

-- aside :: Prism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
-- aside pr = _ . pr . _

-- without :: APrism s t a b -> APrism u v c d -> Prism (Either s u) (Either t v) (Either a c) (Either b d)

-- below :: Traversable f => APrism' s a -> Prism' (f s) (f a)
--
-- isn't :: Prism s t a b -> s -> Bool

-- matching :: APrism s t a b -> s -> Either t a
