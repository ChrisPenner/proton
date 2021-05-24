{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Profunctor.Reader.Class where

import Data.Profunctor
import Data.Function
import Data.Functor.Identity

class (Profunctor p) => ProfunctorReader r p | p -> r where
  {-# MINIMAL (ask | reader), local #-}
  ask :: p a (a, r)
  ask = reader (flip (,))
  reader :: (r -> a -> b) -> p a b
  reader f = rmap (uncurry (flip f)) ask
  local :: (r -> r) -> p a b -> p a b

class (Profunctor p) => ProfunctorReader' r p | p -> r where
  {-# MINIMAL ask', local' #-}
  ask' :: p (a, r) b -> p a b
  local' :: (r -> r) -> p a b -> p a b


type family Result t where
  Result 'True = Identity
  Result 'False = Maybe

newtype Timed (initialized :: Bool) a b = Timed { runTimed :: Int -> a -> Result initialized b }

instance Costrong (Timed 'True) where
  unfirst :: Timed (a, s) (b, s) -> Timed a b
  unfirst (Timed f) = Timed $ \t a ->
      let (b, s) = f t (a, s)
       in b


clock :: Timed 'True () Bool
clock = Timed $ \t () ->
    if t <= 0 then Identity False
              else not $ (runTimed clock) (t - 1) ()


-- instance Costrong Timed where
--   unfirst :: Timed (a, s) (b, s) -> Timed a b
--   unfirst (Timed f) = Timed $ fix (\f' t a -> f' (t - 1) a)

-- instance Cochoice Timed where
--   unleft :: Timed (Either a d) (Either b d) -> Timed a b
--   unleft (Timed f) = Timed $ \t a -> 
--       let loop t' d = 
--               case f t (Right d) of
--                   Left b -> b
--                   Right d' -> loop (t' + 1) d'
--         in case f t (Left a) of
--                 Left b -> b
--                 Right d -> loop t d
