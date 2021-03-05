{-# LANGUAGE ConstraintKinds #-}
module Data.Profunctor.Arrow where

import qualified Control.Category as C
import qualified Control.Arrow as Arr
import Data.Profunctor
import Data.Profunctor.Cayley
import Data.Profunctor.Strong
import Data.Profunctor.Closed
import Data.Profunctor.Choice
import Data.Profunctor.Traversing
import Data.Profunctor.Mapping
import Data.Profunctor.Yoneda
import Data.Profunctor.Ran
import Data.Profunctor.Composition

import Data.Bifunctor.Biff
import Data.Bifunctor.Tannen
import Data.Bifunctor.Joker
import Data.Bifunctor.Product
import Control.Applicative hiding (WrappedArrow(..))


arr :: (Profunctor p, C.Category p) => (a -> b) -> p a b
arr f = rmap f C.id

-- | Split the input between the two argument profunctors and combine their output.
(***) :: (C.Category p, Strong p) => p b c -> p b' c' -> p (b, b') (c, c')
l *** r = first' l C.. second' r

-- | Fanout: send the input to both argument arrows and combine their output.
(&&&) :: (C.Category p, Strong p) => p b c -> p b c' -> p b (c, c')
l &&& r =  lmap (\x -> (x, x)) (l *** r)

-- | Precomposition with a pure function.
(^>>) :: (Profunctor p, C.Category p) => (b -> c) -> p c d -> p b d
f ^>> p = arr f C.>>> p

-- | Postcomposition with a pure function.
(>>^) :: (Profunctor p, C.Category p) => p b c -> (c -> d) -> p b d
p >>^ f = p C.>>> arr f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: (Profunctor p, C.Category p) => p c d -> (b -> c) -> p b d
p <<^ f = p C.<<< arr f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: (Profunctor p, C.Category p) => (c -> d) -> p b c -> p b d
f ^<< p = arr f C.<<< p

(+++) :: (Choice p, C.Category p) => p b c -> p b' c' -> p (Either b b') (Either c c')
l +++ r = left' l C.<<< right' r

(|||) :: (Choice p, C.Category p) => p b d -> p c d -> p (Either b c) d
l ||| r = rmap (either id id) (l +++ r)

class Profunctor p => ProfunctorZero p where
  zeroProfunctor :: p a b

instance Alternative f => ProfunctorZero (Star f) where
  zeroProfunctor = Star (const empty)

instance (Monad m, Alternative m) => ProfunctorZero (Arr.Kleisli m) where
  zeroProfunctor = Arr.Kleisli (const empty)

instance Monoid r => ProfunctorZero (Forget r) where
  zeroProfunctor = Forget (const mempty)

instance (Applicative f, ProfunctorZero p) => ProfunctorZero (Cayley f p) where
  zeroProfunctor = Cayley (pure zeroProfunctor)

instance (Applicative f, ProfunctorZero p) => ProfunctorZero (Tannen f p) where
  zeroProfunctor = Tannen (pure zeroProfunctor)

instance (ProfunctorZero p) => ProfunctorZero (Tambara p) where
  zeroProfunctor = Tambara zeroProfunctor

instance (ProfunctorZero p) => ProfunctorZero (Closure p) where
  zeroProfunctor = Closure zeroProfunctor

instance (ProfunctorZero p) => ProfunctorZero (TambaraSum p) where
  zeroProfunctor = TambaraSum zeroProfunctor

instance (ProfunctorZero p) => ProfunctorZero (CofreeTraversing p) where
  zeroProfunctor = CofreeTraversing zeroProfunctor

instance (ProfunctorZero p) => ProfunctorZero (CofreeMapping p) where
  zeroProfunctor = CofreeMapping zeroProfunctor

instance (ProfunctorZero p) => ProfunctorZero (Yoneda p) where
  zeroProfunctor = Yoneda (\_ _ -> zeroProfunctor)

instance Alternative f => ProfunctorZero (Joker f) where
  zeroProfunctor = Joker empty

instance Arr.ArrowZero p => ProfunctorZero (WrappedArrow p) where
  zeroProfunctor = WrapArrow Arr.zeroArrow

instance ProfunctorZero p => ProfunctorZero (Codensity p) where
  zeroProfunctor = Codensity (const zeroProfunctor)

instance (ProfunctorZero p, ProfunctorZero q) => ProfunctorZero (Product p q) where
  zeroProfunctor = Pair zeroProfunctor zeroProfunctor

instance  (Profunctor p, ProfunctorZero q) => ProfunctorZero (Rift p q) where
  zeroProfunctor = Rift (const zeroProfunctor)

instance  (ProfunctorZero p, Functor f, Functor g) => ProfunctorZero (Biff p f g) where
  zeroProfunctor = Biff zeroProfunctor

class Profunctor p => ProfunctorApply p where
  app :: p (p a b, a) b

instance Functor f => ProfunctorApply (Star f) where
  app = Star (\(Star f, a) -> f a)

instance ProfunctorApply (->) where
  app = Arr.app

instance Monad m => ProfunctorApply (Arr.Kleisli m) where
  app = Arr.app

instance ProfunctorApply (Forget r) where
  app = Forget (\(Forget f, a) -> f a)

instance (Arr.Arrow p, Arr.ArrowApply p) => ProfunctorApply (WrappedArrow p) where
  app = Arr.app

instance Alternative g => ProfunctorApply (Joker g) where
  app = Joker empty
