{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Proton.Indexed where

import Data.Profunctor
import Control.Arrow ((&&&))
import Data.Tuple

data Indexed i p a b = Indexed (p (i, a) b)

instance Profunctor p => Profunctor (Indexed i p) where
  dimap f g (Indexed p) = Indexed (dimap (second' f) g p)

instance Strong p => Strong (Indexed i p) where
  first' (Indexed p) = Indexed (dimap reassoc swap $ second' p)
    where
      reassoc (i, (a, c)) = (c, (i, a))

class (Profunctor p, Profunctor q) => Indexable i p q | p -> q, p -> i where
    indexed :: p a b -> q (i, a) b
    withIndex :: q (i, a) b -> p a b

-- instance Indexable i (Indexed i p) p

indexing :: (Indexable i p q) => (s -> i) -> p s t -> q s t
indexing f p = lmap (f &&& id) $ indexed p
