module Proton.Lens where

import Data.Profunctor
import Data.Profunctor.Strong

type Lens s t a b = forall p. Strong p => p a b -> p s t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = strong setter . lmap getter

view :: Lens s t a b -> s -> a
view l = runForget . l $ Forget id

set :: Lens s t a b -> s -> b -> t
set l s b = l (const b) s

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = l f s
