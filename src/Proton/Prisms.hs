module Proton.Prisms where

import Data.Profunctor
import Proton.Types

type Prism s t a b = forall p. Choice p => p a b -> p s t
type Prism' s a = Prism s s a a

dualPrism :: forall p s t a b. (Choice p, Cochoice p) => (s -> Either t a) -> (b -> Either a t) -> Optic p s t a b
dualPrism l r p = lmap l . go $ rmap r p
  where
    go :: p a (Either a t) -> p (Either t a) t
    go = undefined

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build split = dimap split (either id build) . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' build maybeGet = prism build (\s -> maybe (Left s) Right $ maybeGet s)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) (maybe (Just ()) (const Nothing))

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism Left (either Right (Left . Right))

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism Right (either (Left . Left) Right)

_Show :: (Read a, Show a) => Prism' String a
_Show = prism show $ \s -> case reads s of
  [(a,"")] -> Right a
  _ -> Left s

-- outside :: Representable p => APrism s t a b -> Lens (p t r) (p s r) (p b r) (p a r)

-- aside :: Prism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
-- aside pr = _ . pr . _

-- without :: APrism s t a b -> APrism u v c d -> Prism (Either s u) (Either t v) (Either a c) (Either b d)

-- below :: Traversable f => APrism' s a -> Prism' (f s) (f a)
--
-- isn't :: Prism s t a b -> s -> Bool

-- matching :: APrism s t a b -> s -> Either t a
