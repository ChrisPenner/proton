{-# LANGUAGE InstanceSigs #-}
module Data.Profunctor.DoubleStar where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.MStrong
import Data.Distributive

data DoubleStar f g a b = DoubleStar (f a -> g b)

instance (Functor f, Functor g) => Profunctor (DoubleStar f g) where
  dimap l r (DoubleStar p) = DoubleStar (dimap (fmap l) (fmap r) p)

instance (Traversable f, Distributive g) => MStrong (DoubleStar f g) where
  msecond' :: forall m a b. Monoid m => DoubleStar f g a b -> DoubleStar f g (m, a) (m, b)
  msecond' (DoubleStar p) = DoubleStar (go p)
    where
      go :: (f a -> g b) -> f (m, a) -> g (m, b)
      go f fam = distribute . fmap f . sequenceA $ fam


instance (Traversable f, Distributive g) => Choice (DoubleStar f g) where
  right' :: forall c a b. DoubleStar f g a b -> DoubleStar f g (Either c a) (Either c b)
  right' (DoubleStar p) = DoubleStar go
    where
      go :: f (Either c a) -> g (Either c b)
      go = distribute . fmap p . sequenceA

-- instance (Functor f, Applicative g) => Traversing (DoubleStar f g) where
--   -- traverse' :: Traversable t => p a b -> p (t a) (t b)
--   -- traverse' (DoubleStar p) = DoubleStar (traverse m)
--   wander :: forall s t a b h. Applicative h =>( (a -> h b) -> s -> h t) -> DoubleStar f g a b -> DoubleStar f g s t
--   wander f (DoubleStar p) = DoubleStar go
--     where
--       go :: f s -> g t
--       go fs = _ . fmap (f _p') $ fs

