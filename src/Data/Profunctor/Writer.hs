module Data.Profunctor.Writer where

import Data.Profunctor
import Control.Category (Category)
import qualified Control.Category as C
import Data.Bifunctor (first)
import Data.Profunctor.Writer.Class

newtype WriterT w p a b = WriterT (p a (b, w))

instance Profunctor p => Profunctor (WriterT w p) where
  dimap f g (WriterT w) = WriterT (dimap f (first g) w)

instance (Monoid w, Category p, Strong p) => Category (WriterT w p) where
  id = WriterT (rmap (\b -> (b, mempty)) C.id)
  WriterT x . WriterT y = WriterT (rmap (\((c, w), w') -> (c, w <> w')) (first' x) C.. y)

instance (Profunctor p, Monoid w) => ProfunctorWriter' w (WriterT w p) where
  tell' (WriterT p) = WriterT (rmap (\((b, w), w') -> (b, w <> w')) p)
  listen' (WriterT p) = WriterT (rmap (\(b, w) -> ((b, w), w)) p)
  pass' (WriterT p) = WriterT (rmap (\((b, f), w) -> (b, f w)) p)
