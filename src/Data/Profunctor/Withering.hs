module Data.Profunctor.Withering where

import Data.Profunctor
import Data.Profunctor.Traversing
import Control.Applicative

class (Traversing p) => Withering p where
  cull :: (forall f. Alternative f => (a -> f b) -> (s -> f t)) -> p a b -> p s t

instance Alternative f => Withering (Star f) where
  cull f (Star amb) = Star (f amb)

instance Monoid m => Withering (Forget m) where
  cull f (Forget h) = Forget (getAnnihilation . f (AltConst . Just . h))
    where
      getAnnihilation (AltConst Nothing) = mempty
      getAnnihilation (AltConst (Just m)) = m

newtype AltConst a b = AltConst (Maybe a)
  deriving stock (Eq, Ord, Show, Functor)

instance Monoid a => Applicative (AltConst a) where
  pure _ = (AltConst (Just mempty))
  (AltConst Nothing) <*> _ = (AltConst Nothing)
  _ <*> (AltConst Nothing) = (AltConst Nothing)
  (AltConst (Just a)) <*> (AltConst (Just b)) = AltConst (Just (a <> b))

instance (Semigroup a) => Semigroup (AltConst a x) where
  (AltConst Nothing) <> _ = (AltConst Nothing)
  _ <> (AltConst Nothing) = (AltConst Nothing)
  (AltConst (Just a)) <> (AltConst (Just b)) = AltConst (Just (a <> b))

instance (Monoid a) => Monoid (AltConst a x) where
  mempty = (AltConst (Just mempty))

instance Monoid m => Alternative (AltConst m) where
  empty = (AltConst Nothing)
  (AltConst Nothing) <|> a = a
  a <|> (AltConst Nothing) = a
  (AltConst (Just a)) <|> (AltConst (Just b)) = (AltConst (Just (a <> b)))
