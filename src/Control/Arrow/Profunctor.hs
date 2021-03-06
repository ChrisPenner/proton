{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Control.Arrow.Profunctor where

import Data.Profunctor
import qualified Data.Profunctor.Arrow as PA
import qualified Control.Arrow as Arr
import qualified Control.Category as C
import Data.Coerce
import Data.Bifunctor

newtype WrappedProfunctor p a b = WrappedProfunctor {unwrapProfunctor :: p a b}
  deriving newtype C.Category

instance (Profunctor p, C.Category p, Strong p) => Arr.Arrow (WrappedProfunctor p) where
  arr = WrappedProfunctor . PA.arr
  first (WrappedProfunctor p) = WrappedProfunctor (first' p)
  second (WrappedProfunctor p) = WrappedProfunctor (second' p)
  WrappedProfunctor l *** WrappedProfunctor r = WrappedProfunctor (l PA.*** r)
  WrappedProfunctor l &&& WrappedProfunctor r = WrappedProfunctor (l PA.&&& r)

instance (PA.ProfunctorZero p, C.Category p, Strong p) => Arr.ArrowZero (WrappedProfunctor p) where
  zeroArrow = WrappedProfunctor PA.zeroProfunctor

instance (PA.ProfunctorPlus p, C.Category p, Strong p) => Arr.ArrowPlus (WrappedProfunctor p) where
  WrappedProfunctor l <+> WrappedProfunctor r = WrappedProfunctor (l PA.<+> r)

instance (Choice p, C.Category p, Strong p) => Arr.ArrowChoice (WrappedProfunctor p) where
  left (WrappedProfunctor l) = WrappedProfunctor (left' l)
  right (WrappedProfunctor l) = WrappedProfunctor (right' l)
  WrappedProfunctor l +++ WrappedProfunctor r = WrappedProfunctor (l PA.+++ r)
  WrappedProfunctor l ||| WrappedProfunctor r = WrappedProfunctor (l PA.||| r)

instance (C.Category p, Strong p, PA.ProfunctorApply p) => Arr.ArrowApply (WrappedProfunctor p) where
  app  = WrappedProfunctor (lmap (first coerce) PA.app)

instance (C.Category p, Strong p, Costrong p) => Arr.ArrowLoop (WrappedProfunctor p) where
  loop (WrappedProfunctor p) = WrappedProfunctor (unfirst p)
