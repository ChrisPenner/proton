module Data.Profunctor.Expansive where

import Data.Profunctor
import Control.Applicative
import Data.Foldable
import Data.Tagged
import Data.Profunctor.Cayley

-- Per Reed Mullanix this is "monadicity".
-- We induce a monoid in the structure of the profunctor to collaps our argument.
-- Apparently roughly a T-algebra over some category.
class Expansive p where
  expand :: Foldable f => p a b -> p (f a) b

instance Alternative f => Expansive (Star f) where
  expand (Star f) = Star (asum . fmap f . toList)

instance Monoid r => Expansive (Forget r) where
  expand (Forget f) = Forget (foldMap f)

instance Expansive Tagged where
  expand (Tagged b) = Tagged b

instance (Functor f, Expansive p) => Expansive (Cayley f p) where
  expand (Cayley pfab) = Cayley (fmap expand pfab)
