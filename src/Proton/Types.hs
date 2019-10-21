module Proton.Types where

type Optic p s t a b = p a b -> p s t
type Optic' p s a = Optic p s s a a
