module Proton.Types where

type Optic p s t a b = p a b -> p s t
type Optic' p s a = Optic p s s a a

type Optical p q s t a b = p a b -> q s t
type Optical' p q s a = Optical p q s s a a
