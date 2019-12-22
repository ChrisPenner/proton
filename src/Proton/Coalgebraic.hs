{-# LANGUAGE InstanceSigs #-}
module Proton.Coalgebraic where

import Proton.Types
import Proton.Getter
import Data.Profunctor
import Proton.Algebraic
import Proton.Prisms

type Coalgebraic s t a b = forall p. MChoice p => Optic p s t a b
type Coalgebraic' s a = Coalgebraic s s a a

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

class Profunctor p => MChoice p where
  mleft' :: p a b -> p (Either a m) (Either b m)
  mleft' = dimap swapEither swapEither . mright'
  mright' :: p a b -> p (Either m a) (Either m b)
  mright' = dimap swapEither swapEither . mleft'

instance MChoice (->) where
  mright' = right'

instance Applicative f => MChoice (Star f) where
  mright' = right'

instance (Monoid r) => MChoice (Forget r) where
  mright' = right'

instance Traversable f => MChoice (Costar f) where
  mright' :: forall a b m. Costar f a b -> Costar f (Either m a) (Either m b)
  mright' (Costar f) = (Costar (fmap f . sequenceA))

coprism :: (b -> t) -> (s -> Either t a) -> Coalgebraic s t a b
coprism rev split = dimap split (either id rev) . mright'

coalgPrism :: Prism s t a b -> Coalgebraic s t a b
coalgPrism pr = coprism (review pr) ()

_Just' :: Coalgebraic (Maybe a) (Maybe b) a b
_Just' = coprism Just match
  where
    match (Just a) = Right a
    match Nothing = Left Nothing

_Right' :: Coalgebraic (Either e a) (Either e b) a b
_Right' = coprism Right match
  where
    match (Right a) = Right a
    match (Left e) = Left (Left e)
