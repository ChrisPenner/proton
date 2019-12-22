{-# LANGUAGE InstanceSigs #-}
module Proton.Coalgebraic where

import Proton.Types
import Data.Profunctor

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

coprism :: (m -> t) -> (b -> t) -> (s -> Either m a) -> Coalgebraic s t a b
coprism project rev split =  dimap split (either project rev) . mright'
