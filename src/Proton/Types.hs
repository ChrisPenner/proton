module Proton.Types where

import Data.Profunctor.Indexed
import Data.Profunctor.Coindexed

type Optic p s t a b = p a b -> p s t
type Optic' p s a = Optic p s s a a

type Optical p q s t a b = p a b -> q s t
type Optical' p q s a = Optical p q s s a a
type IndexedOptic i q s t a b = forall p. Indexable i p q => Optical p q s t a b
type IndexedOptic' i p s a = IndexedOptic i p s s a a
type CoindexedOptic e p s t a b = Optical p (Coindexed e p) s t a b
type CoindexedOptic' e p s a = CoindexedOptic e p s s a a
