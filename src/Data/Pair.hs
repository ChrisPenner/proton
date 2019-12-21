module Data.Pair where

import Data.Functor.Rep
import Data.Distributive
import Data.Profunctor

data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor)

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance Distributive Pair where
  distribute = distributeRep

instance Representable Pair where
  type Rep Pair = Bool
  tabulate f = Pair (f False) (f True)
  index (Pair a _) False = a
  index (Pair _ b) True = b

paired :: Profunctor p => p (Pair a) (Pair b) -> p (a, a) (b, b)
paired = dimap (\(a, a') -> Pair a a') (\(Pair a a') -> (a, a'))

liftPair :: (a -> a -> b) -> Pair a -> b
liftPair f (Pair a a') = f a a'
