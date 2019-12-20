{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Proton.Traversal where

import Control.Applicative
import Control.Monad.State
import Data.Bitraversable
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Traversing
import Data.Profunctor.Collapsable
import Proton.Fold
import Proton.Lens
import Proton.Setter
import Proton.Types

type Traversal s t a b = forall p. (Collapsable p, Traversing p) => p a b -> p s t
type Traversal' s a = forall p. Traversing p => p a a -> p s s

traversed :: Traversable f => Traversal (f a) (f b) a b
traversed = traverse'

filtered :: (a -> Bool) -> Traversal' a a
filtered predicate = dimap partition (either id id) . left'
  where
    partition a
        | predicate a = Left a
        | otherwise = Right a

traverseOf :: Optic (Star f) s t a b -> (a -> f b) -> s -> f t
traverseOf t = runStar . t . Star

beside :: forall s t a b s' t' p r. (Representable p, Bitraversable r, Applicative (Rep p)) => Optic p s t a b -> Optic p s' t' a b -> Optic p (r s s') (r t t') a b
beside t1 t2 p = tabulate go
  where
    go :: r s s' -> Rep p (r t t')
    go rss = bitraverse (sieve $ t1 p) (sieve $ t2 p) rss


unsafePartsOf :: forall s t a b. (forall p. p a b -> p s t) -> Lens s t [a] [b]
unsafePartsOf t = lens getter setter'
  where
    getter :: s -> [a]
    getter = toListOf t
    setter' :: s -> [b] -> t
    setter' s bs = flip evalState bs $ traverseOf t insert s
    insert :: x -> State [b] b
    insert _ = gets head <* modify tail

partsOf :: forall s a. (forall p. p a a -> p s s) -> Lens' s [a]
partsOf t = lens getter setter'
  where
    getter :: s -> [a]
    getter = toListOf t
    setter' :: s -> [a] -> s
    setter' s as =
        set (unsafePartsOf t) s (getZipList (ZipList as <|> ZipList (getter s)))

-- taking :: forall s a p. Traversing p => Int -> Optic' p s a -> Optic' p s a
-- taking n t = partsOf t . wander go
--   where
--     go :: Applicative f => (a -> f a) -> [a] -> f [a]
--     go handler as =
--       case splitAt n as of
--         (prefix, suffix) -> liftA2 (<>) (traverse handler prefix) (pure suffix)

-- dropping :: forall s a. Int -> Traversal' s a -> Traversal' s a
-- dropping n t = partsOf t . wander go
--   where
--     go :: Applicative f => (a -> f a) -> [a] -> f [a]
--     go handler as =
--       case splitAt n as of
--         (prefix, suffix) -> liftA2 (<>) (pure prefix) (traverse handler as)


-- failing :: (forall p. Traversing p => p a b -> p s t) -> (forall p. Traversing p => p a b -> p s t) -> (Traversing p => p a b -> p s t)
-- failing f _ pab = undefined $ foldMapOf f (const (Sum 1))
    -- _ $ traverse' @_ @[] p

-- both
-- taking
-- dropping
-- failing ()
