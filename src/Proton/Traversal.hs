{-# LANGUAGE TupleSections #-}
module Proton.Traversal where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Monoid
import Data.Bitraversable
import Control.Applicative

type Traversal s t a b = forall p. Traversing p => p a b -> p s t
type Traversal' s a = forall p. Traversing p => p a a -> p s s

traversed :: Traversable f => Traversal (f a) (f b) a b
traversed = traverse'

filtered :: (a -> Bool) -> Traversal' a a
filtered predicate = dimap partition (either id id) . left'
  where
    partition a
        | predicate a = Left a
        | otherwise = Right a

traverseOf :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf t = runStar . t . Star


beside :: Bitraversable r => Traversal s t a b -> Traversal s' t' a b -> Traversal (r s s') (r t t') a b
beside l1 l2 = wander (liftA2 bitraverse (traverseOf l1) (traverseOf l2))

-- taking :: Int -> Traversal' s a -> Traversal' s a
-- taking n t p = undefined


-- failing :: (forall p. Traversing p => p a b -> p s t) -> (forall p. Traversing p => p a b -> p s t) -> (Traversing p => p a b -> p s t)
-- failing f _ pab = undefined $ foldMapOf f (const (Sum 1))
    -- _ $ traverse' @_ @[] p

-- both
-- taking
-- dropping
-- partsOf
-- failing ()
