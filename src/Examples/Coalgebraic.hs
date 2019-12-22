module Examples.Coalgebraic where

-- >>> [Just 1, Just 2, Just 3] & _Just >- sum
-- Just 6
-- >>> [Just 1, Nothing, Just 3] & _Just >- sum
-- Nothing
-- >>> [Right 1, Left "whoops", Right 2] & _Right >- sum
-- Left "whoops"
