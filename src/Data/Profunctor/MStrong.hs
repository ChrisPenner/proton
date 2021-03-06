{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.MStrong where

import Data.Profunctor
import Data.Tagged
import Data.Tuple
import Data.Foldable

class Profunctor p => MStrong p where
  mfirst' ::  Monoid m => p a b -> p (a, m) (b, m)
  mfirst' = dimap swap swap . msecond'
  msecond' ::  Monoid m => p a b -> p (m, a) (m, b)
  msecond' = dimap swap swap . mfirst'

  {-# MINIMAL mfirst' | msecond' #-}

instance MStrong (Forget r) where
  msecond' = second'

instance MStrong (->) where
  msecond' = second'

instance Functor f => MStrong (Star f) where
  msecond'  = second'

instance MStrong Tagged where
  msecond' (Tagged b) = Tagged (mempty, b)

instance (Functor f, Foldable f) => MStrong (Costar f) where
  msecond' (Costar f) = Costar (\fma -> (fold (fmap fst fma), f (fmap snd fma)))
