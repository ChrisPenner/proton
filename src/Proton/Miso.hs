module Proton.Miso where

import Data.Profunctor

data Miso m a b s t = Miso (s -> m a) (b -> m t)

instance Functor m => Profunctor (Miso m a b) where
  dimap l r (Miso sma bmt) = Miso (sma . l) (fmap r . bmt)
