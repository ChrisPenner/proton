{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module FoldM where
import Data.Profunctor

-- https://hackage.haskell.org/package/foldl-1.4.10/docs/Control-Foldl.html#t:FoldM
data FoldM m a b = forall x. FoldM (x -> a -> m x) (m x) (x -> m b)

instance Functor m => Profunctor (FoldM m) where
  dimap l r (FoldM step initial extract) = FoldM (\x a -> step x (l a)) initial (fmap r . extract)

-- instance Choice (FoldM m) where
--   right' (FoldM step initial extract) = FoldM (\x a -> )
--   where
--     step' x (Left l) 
--     step' x (Right)
