module Data.Profunctor.Utils where

import Data.Profunctor
import Data.Profunctor.NonLinear
import Control.Category ((>>>), Category)


class Branch p where
  branch :: p a b -> p b Bool -> p a (Either b b)

choose :: (Category p, Branch p, NonLinear p, Strong p) => p b Bool -> p a b -> p a (Either b b)
choose predicate p = branch p predicate

