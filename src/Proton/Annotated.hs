{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Proton.Annotated where

import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.Coindexed
import Proton.Types
import Proton.Setter


-- coindexing :: (Coindexable i p q) => (s -> i) -> p s t -> q s t
-- coindexing f p = lmap (f &&& id) $ coindexed p

-- itraversed :: (Coindexable Int p q, Traversing q) => p a b -> q [a] [b]
-- vFirst :: CoindexedOptic String p [a] [b] a b
-- vFirst p = _ $ coindexed p

vView :: CoindexedOptic e (Forget (Either e a)) s t a b -> s -> Either e a
vView lns = runForget . runCoindexed $ lns (Forget Right)

-- vToListOf :: CoindexedOptic e (Forget (Either e a)) s t a b -> s -> Either e a
-- vToListOf lns = runForget . runCoindexed $ lns (Forget Right)

coindexing :: forall e p s t a b. Profunctor p =>
    Optic (Coindexed e p) s t a b -> Optic p s (Either e t) a b
coindexing o p = runCoindexed . o $ Coindexed (rmap Right p)

-- itoListOf :: CoindexedOptic e (Forget [a]) s t a b -> s -> [(i, a)]
-- itoListOf fld = _ (fld (Forget pure))

vOver :: Optic (Coindexed e (->)) s t a b -> (a -> b) -> s -> Either e t
vOver modifier f s = over (coindexing modifier) f s

-- vFirst :: forall p a. Choice p => p a a -> Coindexed String p [a] [a]
-- vFirst p = Coindexed (dimap _ _ $ right' p)
--   where
--     -- passThrough :: p (Either [a] (a, [a])) (Either [a] (a, [a]))
--     passThrough = dimap _ _ $ right' p

vFirst :: forall p a. Traversing p => p a a -> Coindexed String p [a] [a]
vFirst p = Coindexed (wander go p)
  where
    go :: forall f. Applicative f => (a -> f a) -> [a] -> f (Either String [a])
    go _ [] = pure (Left "No first element to list")
    go f (x : xs) = Right . (:xs) <$> f x

-- iset :: CoindexedOptic i (->) s t a b -> (i -> b) -> s -> t
-- iset setter f = iover setter (\i _ -> f i)
