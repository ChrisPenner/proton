{-# LANGUAGE UndecidableInstances #-}
module Data.Profunctor.NonLinear where

import Data.Profunctor

-- A Class for providing explicit duplication support for arrow-likes 
-- that don't support Dimap using (->)
class NonLinear p where
  dup :: p a b -> p a (b, b)

-- An overlappable instance for all profunctors.
-- This should be overridden if nonlinearity has special meaning in your domain
instance {-# OVERLAPPABLE #-} Profunctor p => NonLinear p where
  dup = rmap (\a -> (a, a))
