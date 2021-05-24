module Data.Profunctor.Reader where

import Data.Profunctor
import Control.Category (Category)
import qualified Control.Category as C
import Data.Bifunctor (first, second)
import Data.Profunctor.Reader.Class
import Data.Profunctor.Strong

newtype ReaderT r p a b = ReaderT (p (a, r) b)

instance Profunctor p => Profunctor (ReaderT r p) where
  dimap f g (ReaderT r) = ReaderT (dimap (first f) (g) r)

instance (Category p, Strong p) => Category (ReaderT r p) where
  id = ReaderT (lmap fst C.id)
  ReaderT x . ReaderT y = ReaderT (x C.. strong (\(_, r) b -> (b, r)) y)

instance (Profunctor p, Category p) => ProfunctorReader r (ReaderT r p) where
  ask = ReaderT C.id
  reader f = rmap (uncurry $ flip f) ask
  local f (ReaderT q) = ReaderT (lmap (second f) q)

instance (Profunctor p) => ProfunctorReader' r (ReaderT r p) where
  ask' (ReaderT p) = ReaderT $ lmap (\(a, r) -> ((a, r), r)) p
  local' f (ReaderT p) = ReaderT $ lmap (second f) p
