module Data.Profunctor.Writer where

import Data.Profunctor
import Control.Category (Category)
import qualified Control.Category as C
import Data.Bifunctor (first, second)
import Data.Profunctor.Writer.Class

newtype WriterT w p a b = WriterT (p (a, w) (b, w))

instance Profunctor p => Profunctor (WriterT w p) where
  dimap f g (WriterT w) = WriterT (dimap (first f) (first g) w)

instance (Category p) => Category (WriterT w p) where
  id = WriterT C.id
  WriterT w . WriterT t = WriterT (w C.. t)

instance (Profunctor p, Category p, Monoid w) => ProfunctorWriter w (WriterT w p) where
  writer w = WriterT (rmap (second (<> w)) C.id)
  tell = WriterT (rmap (\((a, w'), w) -> (a, w <> w')) C.id)
  listen (WriterT p) = WriterT (rmap (\(b, w) -> ((b, w), w)) p)
  pass (WriterT p) = WriterT (rmap (\((b, f), w) -> (b, f w)) p)
