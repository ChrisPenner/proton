module Data.Profunctor.Traced where

import Data.Profunctor
import Data.Bifunctor (first)

data Traced m a b = Traced ((a, m) -> b)

instance Profunctor (Traced m) where
  dimap f g (Traced t) = Traced (g . t . first f)

instance Strong (Traced m) where
  first' (Traced t) = Traced (first' t . reassoc)
    where
      reassoc ((a, c), m) = ((a, m), c)

instance Choice (Traced m) where
  left' (Traced t) = Traced (left' t . reassoc)
    where
      reassoc (Left a, m) = Left (a, m)
      reassoc (Right c, _) = Right c

extractTraced :: Monoid m => Traced m a b -> a -> b
extractTraced (Traced t) a = t (a, mempty)

-- extend :: Semigroup m => (Traced m x a -> b) -> Traced m x a -> Traced m x b
-- extend f (Traced t) = Traced go
--   where
--     go (x, m) = f $ Traced (\(x, m') -> t (x, m <> m'))
