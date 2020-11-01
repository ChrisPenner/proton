{-# LANGUAGE ScopedTypeVariables #-}
module Proton.Wither where

import Data.Profunctor
import Data.Profunctor.Withering
import Control.Applicative
import Proton.Types
import Proton.Prisms

type Wither s t a b = forall p. Withering p => Optic p s t a b
type Wither' s a = Wither s s a a

-- type Selector' s a = Selector s s a a
-- type Selector s t a b = forall p. (Withering p, Depending p) => Optic p s t a b


guarding :: Alternative f => (a -> Bool) -> a -> f a
guarding p a
    | p a = pure a
    | otherwise = empty

guarded :: forall a b. (a -> Bool) -> Wither a b a b
guarded p = cull guarded'
  where
    guarded' :: forall f. Alternative f => (a -> f b) -> a -> f b
    guarded' f a
      | p a = f a
      | otherwise = empty

-- selectResult :: forall a b. (b -> Bool) -> Selector a b a b
-- selectResult p = cull _collapse . depend check
--   where
--     -- collapse :: forall f. Alternative f => (a -> f (b)) -> a -> f b
--     check :: forall f. Monad f => (a -> f b) -> a -> f (Maybe b)
--     check f a = do
--         f a >>= \b ->
--             if p b then pure $ Just b
--                    else pure $ Nothing

filterOf :: Optic (Star Maybe) s t a a -> (a -> Bool) -> s -> Maybe t
filterOf w p s = runStar (w (Star (guarding p))) s

witherPrism :: forall p s t a b. Withering p => Prism s t a b -> Optic p s t a b
witherPrism prsm =
    withPrism prsm $ \embed match ->
      let
        go :: Alternative f => (a -> f b) -> s -> f t
        go f s = 
            case match s of
                Left _ -> empty
                Right a -> fmap embed $ f a
      in cull go
