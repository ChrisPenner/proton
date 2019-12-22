module Data.Profunctor.Expanding where

import Data.Profunctor
import Control.Comonad

class Profunctor p => Expanding p where
  expand :: Comonad w => p (w a) b -> p a (w b)
  -- expand :: Applicative f => p (f a) (f b) -> p a b

-- instance Expanding (->) where
--   expand f = _ f

-- instance Expanding (Forget r) where
--   expand (Forget rToA) = Forget (rToA . pure)

-- instance Expanding (Forget r) where
--   expand (Forget rToA) = Forget (_ rToA)


-- instance Functor g => Expanding (Star g) where
--   expand (Star f) = Star (_ f)

-- instance Functor g => Expanding (Costar g) where
--   expand (Costar f) = Costar (_ f)


-- extending :: (Comonad w) => Optic (Costar w) a (w b) a b
-- extending (Costar f) = Costar (extend f)
