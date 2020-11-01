module Data.Profunctor.Remember where

import Data.Profunctor

newtype Remember r a b = Remember (r -> b)
  deriving Functor

instance Profunctor (Remember r) where
  dimap _ g (Remember x) = Remember (g . x)

instance Choice (Remember r) where
  left' (Remember x) = Remember (Left . x)

instance Closed (Remember r) where
  closed (Remember x) = Remember (\r _ -> x r)

instance Costrong (Remember r) where
  unfirst (Remember x) = Remember (fst . x)
