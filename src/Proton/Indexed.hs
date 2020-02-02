{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Proton.Indexed where

import Data.Profunctor
import Data.Profunctor.Traversing
import Control.Arrow ((&&&))
import Data.Profunctor.Indexed
import Proton.Types

indexing :: (Indexable i p q) => (s -> i) -> p s t -> q s t
indexing f p = lmap (f &&& id) $ indexed p

-- itraversed :: (Indexable Int p q, Traversing q) => p a b -> q [a] [b]
itraversed :: Traversing p => IndexedOptic Int p [a] [b] a b
itraversed p = lmap (zip [(0 :: Int)..]) . traverse' $ indexed p

itoListOf :: IndexedOptic i (Forget [(i, a)]) s t a b -> s -> [(i, a)]
itoListOf fld = runForget (fld (Indexed (Forget pure)))

iover :: IndexedOptic i (->) s t a b -> (i -> a -> b) -> s -> t
iover setter f = setter (Indexed (uncurry f))

iset :: IndexedOptic i (->) s t a b -> (i -> b) -> s -> t
iset setter f = iover setter (\i _ -> f i)
