{-# LANGUAGE TupleSections #-}
module Proton.Traversal where

import Proton.Fold
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Monoid

type Traversal s t a b = forall p. Traversing p => p a b -> p s t
type Traversal' s a = forall p. Traversing p => p a a -> p s s

traversed :: Traversal [a] [b] a b
traversed = traverse'

filtered :: (a -> Bool) -> Traversal' a a
filtered predicate = dimap partition (either id id) . left'
  where
    partition a
        | predicate a = Left a
        | otherwise = Right a

traverseOf :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf t = runStar . t . Star

-- taking :: Int -> Traversal' s a -> Traversal' s a
-- taking n t p = undefined


-- failing :: (forall p. Traversing p => p a b -> p s t) -> (forall p. Traversing p => p a b -> p s t) -> (Traversing p => p a b -> p s t)
-- failing f _ pab = undefined $ foldMapOf f (const (Sum 1))
    -- _ $ traverse' @_ @[] p

-- both
-- beside
-- taking
-- dropping
-- partsOf
-- failing ()
