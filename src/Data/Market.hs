module Data.Market where

import Data.Profunctor
import Data.Bifunctor

-- | The `Market` profunctor characterizes a `Prism`.
data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market proj match) = Market (f . proj) (first f . match)

instance Profunctor (Market a b) where
  dimap f g (Market a b) = Market (g . a) (first g . b . f)

instance Choice (Market a b) where
  left' (Market x y) =
    Market (Left . x) (either (first Left . y) (Left . Right))
  right' (Market x y) =
    Market (Right . x) (either (Left . Left) (first Right . y))
